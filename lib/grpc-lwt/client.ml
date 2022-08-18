open Lwt.Syntax

type response_handler = H2.Client_connection.response_handler

type do_request =
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

let call ~service ~rpc ?(scheme = "https") ~handler ~(do_request : do_request)
    ?(headers = default_headers) () =
  let request = make_request ~service ~rpc ~scheme ~headers in
  let read_body, read_body_notify = Lwt.task () in
  let response, response_notify = Lwt.task () in
  let status, status_notify = Lwt.task () in
  let trailers_handler headers =
    let code =
      match H2.Headers.get headers "grpc-status" with
      | None -> None
      | Some s -> (
          match int_of_string_opt s with
          | None -> None
          | Some i -> Grpc.Status.code_of_int i)
    in
    match code with
    | None -> ()
    | Some code -> (
        match Lwt.state status with
        | Sleep ->
            let message = H2.Headers.get headers "grpc-message" in
            let status = Grpc.Status.v ?message code in
            Lwt.wakeup_later status_notify status
        | _ -> (* This should never happen, but just in case. *) ())
  in
  let response_handler (response : H2.Response.t) body =
    Lwt.wakeup_later read_body_notify body;
    Lwt.wakeup_later response_notify response;
    trailers_handler response.headers
  in
  let write_body = do_request request ~response_handler ~trailers_handler in
  let* handler_res = handler write_body read_body in
  let* response = response in
  let+ status =
    match Lwt.is_sleeping status with
    (* In case no grpc-status appears in headers or trailers. *)
    | false -> status
    | true ->
        Lwt.return
          (Grpc.Status.v ~message:"Server did not return grpc-status"
             Grpc.Status.Unknown)
  in
  let out =
    if response.status <> `OK then Error (Grpc.Status.v Grpc.Status.Unknown)
    else Ok handler_res
  in
  match out with Error _ as e -> e | Ok out -> Ok (out, status)

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

  let unary ~f enc =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        (fun enc ->
          encoder_push (Some enc);
          encoder_push None)
          enc;
        let decoder = Lwt_stream.get decoder_stream in
        f decoder)
end
