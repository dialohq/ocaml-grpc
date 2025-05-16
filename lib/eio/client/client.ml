open Grpc_eio_core

type context = { sent_all : bool; parse_state : Body_parse.state }

let fill_header_cs ~length (buffer : Cstruct.t) =
  Cstruct.set_char buffer 0 '\x00';
  Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
  Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)

module Unary = struct
  let call ~(channel : context Channel.t) ~service ~method_name ~headers encode
      : (Pbrt.Decoder.t, Status_new.t) result =
    let pool = Buffer_pool.Bytes_pool.make () in

    (* sending request *)
    let encoder = Pbrt.Encoder.create ~size:1_000 () in
    Pbrt.Encoder.clear encoder;
    encode encoder;
    let length = Pbrt.Encoder.length encoder in

    let header_buffer = Cstruct.create 5 in
    fill_header_cs ~length header_buffer;

    let body_buffer = Cstruct.create length in
    Pbrt.Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
      body_buffer 0;

    let request_data = [ header_buffer; Cstruct.sub body_buffer 0 length ] in

    let data_writer : context Channel.data_writer =
     fun c ->
      if c.sent_all then (None, c)
      else (Some request_data, { c with sent_all = true })
    in
    (* ****************** *)

    (* receiving response *)
    let decoder_promise, decoder_resolver = Eio.Promise.create () in

    let data_receiver : context Channel.data_receiver =
     fun c -> function
      | Some data ->
          let parse_state, parsed =
            Body_parse.read_messages ~pool data c.parse_state
          in

          if not (List.is_empty parsed) then
            Eio.Promise.resolve decoder_resolver
              (Pbrt.Decoder.of_bytes @@ List.hd parsed);

          { c with parse_state }
      | None -> c
    in
    (* ****************** *)

    let path = Grpc_client.make_path ~service ~method_name in
    let initial_context = { sent_all = false; parse_state = Idle } in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | { code = OK; _ } -> Ok (Eio.Promise.await decoder_promise)
    | status -> Error status
end
