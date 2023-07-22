open! Core
open! Async

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

let trailers_handler trailers_status_ivar headers =
  Ivar.fill trailers_status_ivar (Grpc.Status.extract_status headers)

let response_handler read_body_ivar out_ivar (response : H2.Response.t)
    (body : H2.Body.Reader.t) =
  Ivar.fill read_body_ivar body;
  Ivar.fill out_ivar response

let call ~service ~rpc ?(scheme = "https") ~handler ~do_request
    ?(headers = default_headers) () =
  let request = make_request ~service ~rpc ~scheme ~headers in
  let read_body_ivar = Ivar.create () in
  let out_ivar = Ivar.create () in
  let trailers_status_ivar = Ivar.create () in
  let write_body : H2.Body.Writer.t =
    do_request ?flush_headers_immediately:None
      ?trailers_handler:(Some (trailers_handler trailers_status_ivar))
      request
      ~response_handler:(response_handler read_body_ivar out_ivar)
  in
  let%bind handler_res = handler write_body (Ivar.read read_body_ivar) in
  let%bind response = Ivar.read out_ivar in
  let out =
    match response.status with
    | `OK -> Ok (handler_res, response.headers)
    | _ -> Error (Grpc.Status.extract_status response.headers)
  in
  match out with
  | Error _ as e -> return e
  | Ok (out, headers) ->
      let%bind status =
        match H2.Headers.get headers "grpc-status" with
        | Some _ -> return (Grpc.Status.extract_status headers)
        | None -> Ivar.read trailers_status_ivar
      in
      return (Ok (out, status))

module Rpc = struct
  type 'a handler =
    H2.Body.Writer.t -> H2.Body.Reader.t Deferred.t -> 'a Deferred.t

  let bidirectional_streaming ~handler write_body read_body =
    let decoder_r, decoder_w = Async.Pipe.create () in
    don't_wait_for
      (let%map read_body = read_body in
       Connection.grpc_recv_streaming read_body decoder_w);
    let encoder_r, encoder_w = Async.Pipe.create () in
    don't_wait_for (Connection.grpc_send_streaming_client write_body encoder_r);
    let%bind out = handler encoder_w decoder_r in
    if not (Pipe.is_closed encoder_w) then Pipe.close encoder_w;
    if not (Pipe.is_closed decoder_w) then Pipe.close decoder_w;
    return out

  let client_streaming ~handler write_body read_body =
    bidirectional_streaming
      ~handler:(fun encoder_w decoder_r ->
        handler encoder_w
          (match%map Async.Pipe.read decoder_r with
          | `Eof -> None
          | `Ok a -> Some a))
      write_body read_body

  let server_streaming ~handler ~encoded_request write_body read_body =
    bidirectional_streaming
      ~handler:(fun encoder_w decoder_r ->
        Async.Pipe.write_without_pushback encoder_w encoded_request;
        Async.Pipe.close encoder_w;
        handler decoder_r)
      write_body read_body

  let unary ~handler ~encoded_request write_body read_body =
    let payload = Grpc.Message.make encoded_request in
    H2.Body.Writer.write_string write_body payload;
    H2.Body.Writer.close write_body;
    let%bind read_body = read_body in
    let request_buffer = Grpc.Buffer.v () in
    let on_eof () = () in
    let rec on_read buffer ~off ~len =
      Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src:buffer
        ~dst:request_buffer ~length:len;
      H2.Body.Reader.schedule_read read_body ~on_read ~on_eof
    in
    H2.Body.Reader.schedule_read read_body ~on_read ~on_eof;
    handler (Grpc.Message.extract request_buffer)
end
