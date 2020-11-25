let make_request ?(scheme = "https") ~service ~rpc =
  let request =
    H2.Request.create ~scheme `POST
      ("/" ^ service ^ "/" ^ rpc)
      ~headers:
        H2.Headers.(
          add_list empty
            [ ("te", "trailers"); ("content-type", "application/grpc+proto") ])
  in
  (* TODO: find service and return handler for correct rpc within *)
  request

module Rpc = struct
  open Lwt.Syntax

  type unary = Pbrt.Encoder.t -> Pbrt.Decoder.t

  type bidirectional_streaming =
    unit -> (Pbrt.Encoder.t -> unit) * Pbrt.Decoder.t Lwt_stream.t

  let bidirectional_streaming ~f write_body _response read_body =
    let* write_body = write_body in
    let decoder_stream, decoder_push = Lwt_stream.create () in
    Connection.grpc_recv_streaming read_body decoder_push;
    let encoder_stream, encoder_push = Lwt_stream.create () in
    Lwt.async (fun () ->
        Connection.grpc_send_streaming_client write_body encoder_stream);
    let+ () = f (fun encoder -> encoder_push (Some encoder)) decoder_stream in
    encoder_push None

  let client_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun encoder_push decoder_stream ->
        let+ encoder = f decoder_stream in
        match encoder with None -> () | Some encoder -> encoder_push encoder)

  let server_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun encoder_push decoder_stream ->
        let* decoder = Lwt_stream.get decoder_stream in
        match decoder with
        | None -> Lwt.return_unit
        | Some decoder -> f encoder_push decoder)

  let unary ~f reqd =
    bidirectional_streaming reqd ~f:(fun encoder_push decoder_stream ->
        let* decoder = Lwt_stream.get decoder_stream in
        match decoder with
        | None -> Lwt.return_unit
        | Some decoder ->
            let+ status, encoder = f decoder in
            ( match encoder with
            | None -> ()
            | Some encoder -> encoder_push encoder );
            status)
end
