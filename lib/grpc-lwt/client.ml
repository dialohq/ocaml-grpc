open Lwt.Syntax

type response_handler = H2.Client_connection.response_handler

type do_request =
  ?flush_headers_immediately:bool ->
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  H2.Body.Writer.t

let make_request ~scheme ~service ~rpc ~headers =
  let request =
    H2.Request.create ~scheme `POST ("/" ^ service ^ "/" ^ rpc) ~headers
  in
  request

let default_headers =
  H2.Headers.of_list
    [ ("te", "trailers"); ("content-type", "application/grpc+proto") ]

let trailers_handler status_notify headers =
  Lwt.wakeup_later status_notify (Grpc.Status.extract_status headers)

let response_handler read_body_notify response_notify (response : H2.Response.t)
    body =
  Lwt.wakeup_later read_body_notify body;
  Lwt.wakeup_later response_notify response

let call ~service ~rpc ?(scheme = "https") ~handler ~(do_request : do_request)
    ?(headers = default_headers) () =
  let request = make_request ~service ~rpc ~scheme ~headers in
  let flush_headers_immediately = None in
  let read_body, read_body_notify = Lwt.task () in
  let response, response_notify = Lwt.task () in
  let status, status_notify = Lwt.task () in
  let write_body =
    do_request ?flush_headers_immediately request
      ~response_handler:(response_handler read_body_notify response_notify)
      ~trailers_handler:(trailers_handler status_notify)
  in
  let* handler_res = handler write_body read_body in
  let* response = response in
  let out =
    if response.status <> `OK then
      Error (Grpc.Status.extract_status response.headers)
    else Ok (handler_res, response.headers)
  in
  match out with
  | Error _ as e -> Lwt.return e
  | Ok (out, headers) ->
      let+ status =
        match H2.Headers.get headers "grpc-status" with
        | Some _ -> Lwt.return (Grpc.Status.extract_status headers)
        | None -> status
      in
      Ok (out, status)

module Rpc = struct
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t Lwt.t -> 'a Lwt.t

  let bidirectional_streaming ~f write_body read_body =
    let encoder_stream, encoder_push = Lwt_stream.create () in
    let decoder_stream, decoder_push = Lwt_stream.create () in
    let res = f encoder_push decoder_stream in
    let* () =
      Lwt.join
        [
          Connection.grpc_send_streaming_client write_body encoder_stream;
          (let+ read_body = read_body in
           Connection.grpc_recv_streaming read_body decoder_push);
        ]
    in
    res

  let client_streaming ~f =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        let decoder = Lwt_stream.get decoder_stream in
        f encoder_push decoder)

  let server_streaming ~f enc =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        (fun enc ->
          encoder_push (Some enc);
          encoder_push None)
          enc;
        f decoder_stream)

  let unary ~f enc write_body read_body =
    let payload = Grpc.Message.make enc in
    H2.Body.write_string write_body payload;
    H2.Body.close_writer write_body;
    let* read_body = read_body in
    let request_buffer = Grpc.Buffer.v () in
    let message, message_notify = Lwt.task () in
    let on_eof () =
      let message = Grpc.Message.extract request_buffer in
      Lwt.wakeup_later message_notify message
    in
    let rec on_read buffer ~off ~len =
      Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src:buffer
        ~dst:request_buffer ~length:len;
      H2.Body.schedule_read read_body ~on_read ~on_eof
    in
    H2.Body.schedule_read read_body ~on_read ~on_eof;
    f message
end
