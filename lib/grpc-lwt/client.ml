open Lwt.Syntax

let make_request ~scheme ~service ~rpc =
  let request =
    H2.Request.create ~scheme `POST
      ("/" ^ service ^ "/" ^ rpc)
      ~headers:
        H2.Headers.(
          add_list empty
            [ ("te", "trailers"); ("content-type", "application/grpc+proto") ])
  in
  request

let call ?(error_handler = fun _ -> ()) ~service ~rpc ?(scheme = "https")
    ~handler ~do_request () =
  let request = make_request ~service ~rpc ~scheme in
  let write_body, write_body_notify = Lwt.wait () in
  let out, notify_out = Lwt.wait () in
  let response_handler response body =
    Lwt.async (fun () ->
        let+ out = handler ~write_body response body in
        Lwt.wakeup_later notify_out out)
  in
  let body = do_request request ~error_handler ~response_handler in
  Lwt.wakeup_later write_body_notify body;
  out

module Rpc = struct
  let bidirectional_streaming ~f ~write_body _response read_body =
    let* write_body = write_body in
    let decoder_stream, decoder_push = Lwt_stream.create () in
    Connection.grpc_recv_streaming read_body decoder_push;
    let encoder_stream, encoder_push = Lwt_stream.create () in
    Lwt.async (fun () ->
        Connection.grpc_send_streaming_client write_body encoder_stream);
    let+ out = f (fun encoder -> encoder_push (Some encoder)) decoder_stream in
    encoder_push None;
    out

  let client_streaming ~f =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        let decoder = Lwt_stream.get decoder_stream in
        f encoder_push decoder)

  let server_streaming ~f enc =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        encoder_push enc;
        f decoder_stream)

  let unary ~f enc =
    bidirectional_streaming ~f:(fun encoder_push decoder_stream ->
        encoder_push enc;
        let* decoder = Lwt_stream.get decoder_stream in
        f decoder)
end
