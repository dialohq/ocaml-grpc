open Lwt.Syntax

type unary = Pbrt.Decoder.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t

type client_streaming =
  Pbrt.Decoder.t Lwt_stream.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t

type server_streaming =
  Pbrt.Decoder.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t

type bidirectional_streaming =
  Pbrt.Decoder.t Lwt_stream.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t

type t =
  | Unary of unary
  | Client_streaming of client_streaming
  | Server_streaming of server_streaming
  | Bidirectional_streaming of bidirectional_streaming

let bidirectional_streaming ~f ~reqd =
  let decoder_stream, decoder_push = Lwt_stream.create () in
  Connection.grpc_recv_streaming reqd decoder_push;
  let encoder_stream, encoder_push = Lwt_stream.create () in
  let status_mvar = Lwt_mvar.create_empty () in
  Lwt.async (fun () ->
      Connection.grpc_send_streaming reqd encoder_stream status_mvar);
  let* status = f decoder_stream (fun encoder -> encoder_push (Some encoder)) in
  encoder_push None;
  Lwt_mvar.put status_mvar status

let client_streaming ~f ~reqd =
  bidirectional_streaming ~reqd ~f:(fun decoder_stream encoder_push ->
      let+ status, encoder = f decoder_stream in
      (match encoder with None -> () | Some encoder -> encoder_push encoder);
      (status : Grpc.Status.t))

let server_streaming ~f ~reqd =
  bidirectional_streaming ~reqd ~f:(fun decoder_stream encoder_push ->
      let* decoder = Lwt_stream.get decoder_stream in
      match decoder with
      | None -> Lwt.return Grpc.Status.(v OK)
      | Some decoder -> f decoder encoder_push)

let unary ~f ~reqd =
  bidirectional_streaming ~reqd ~f:(fun decoder_stream encoder_push ->
      let* decoder = Lwt_stream.get decoder_stream in
      match decoder with
      | None -> Lwt.return Grpc.Status.(v OK)
      | Some decoder ->
          let+ status, encoder = f decoder in
          ( match encoder with
          | None -> ()
          | Some encoder -> encoder_push encoder );
          status)
