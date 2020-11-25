let make_request ?(scheme = "https") ~service ~rpc =
  let request =
    H2.Request.create ~scheme `POST
      ("/" ^ service ^ "/" ^ rpc)
      ~headers:
        H2.Headers.(
          add_list empty
            [ ("te", "trailers"); ("content-type", "application/grpc+proto") ])
  in
  request

module Rpc = struct
  open Lwt.Syntax

  let bidirectional_streaming ~f write_body encoder _response read_body =
    let* write_body = write_body in
    H2.Body.write_string write_body (Pbrt.Encoder.to_string encoder);
    let decoder_stream, decoder_push = Lwt_stream.create () in
    Connection.grpc_recv_streaming read_body decoder_push;
    let encoder_stream, encoder_push = Lwt_stream.create () in
    Lwt.async (fun () ->
        Connection.grpc_send_streaming_client write_body encoder_stream);
    let+ () = f (fun encoder -> encoder_push (Some encoder)) decoder_stream in
    encoder_push None

  let client_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun encoder_push decoder_stream ->
        let decoder = Lwt_stream.get decoder_stream in
        f encoder_push decoder)

  let server_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun _ decoder_stream -> f decoder_stream)

  let unary ~f reqd =
    bidirectional_streaming reqd ~f:(fun _ decoder_stream ->
        let* decoder = Lwt_stream.get decoder_stream in
        match decoder with None -> Lwt.return_unit | Some decoder -> f decoder)
end
