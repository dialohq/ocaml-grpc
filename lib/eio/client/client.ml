open Grpc_eio_core

type read_state = Reading | Done of Pbrt.Decoder.t

type context = {
  sent_all : bool;
  read_state : read_state;
  parse_state : Body_parse.state;
}

let fill_header_cs ~length (buffer : Cstruct.t) =
  Cstruct.set_char buffer 0 '\x00';
  Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
  Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)

let single_write encode_request : context Channel.data_writer =
  let encoder = Pbrt.Encoder.create ~size:1_000 () in
  Pbrt.Encoder.clear encoder;
  encode_request encoder;
  let length = Pbrt.Encoder.length encoder in

  let header_buffer = Cstruct.create 5 in
  fill_header_cs ~length header_buffer;

  let body_buffer = Cstruct.create length in
  Pbrt.Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
    body_buffer 0;

  fun context ->
    if context.sent_all then (None, context)
    else (Some [ header_buffer; body_buffer ], { context with sent_all = true })

let single_read : context Channel.data_receiver =
 fun context data ->
  match context.read_state with
  | Done _ -> { action = `Reset; context }
  | Reading ->
      let new_parse_state, parsed =
        Body_parse.read_messages data context.parse_state
      in
      let read_state =
        match parsed with
        | [] -> Reading
        | x :: _ -> Done (Pbrt.Decoder.of_bytes x)
      in
      {
        action = `Continue;
        context = { context with read_state; parse_state = new_parse_state };
      }

let multi_write ~encoder ~header_buffer write_handler :
    context Channel.data_writer =
 fun context ->
  match write_handler () with
  | None -> (None, { context with sent_all = true })
  | Some encode_request ->
      Pbrt.Encoder.clear encoder;
      encode_request encoder;
      let length = Pbrt.Encoder.length encoder in

      fill_header_cs ~length header_buffer;

      let body_buffer = Cstruct.create length in
      Pbrt.Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
        encoder body_buffer 0;
      (Some [ header_buffer; body_buffer ], context)

let multi_read read_handler : context Channel.data_receiver =
 fun context data ->
  let parse_state, parsed = Body_parse.read_messages data context.parse_state in

  List.iter (fun msg -> read_handler (Pbrt.Decoder.of_bytes msg)) parsed;

  { action = `Continue; context = { context with parse_state } }

module Unary = struct
  let call :
      channel:context Channel.t ->
      service:string ->
      method_name:string ->
      headers:Grpc_client.request_headers ->
      (Pbrt.Encoder.t -> unit) ->
      (Pbrt.Decoder.t, Status_new.t) result =
   fun ~channel ~service ~method_name ~headers encode_request ->
    let data_writer = single_write encode_request in

    let data_receiver = single_read in

    let path = Grpc_client.make_path ~service ~method_name in
    let initial_context =
      { sent_all = false; read_state = Reading; parse_state = Idle }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | {
     status = { code = OK; _ };
     grpc_context = { read_state = Done decoder; _ };
    } ->
        Ok decoder
    | { status = { code = OK; _ }; grpc_context = { read_state = Reading; _ } }
      ->
        Error
          {
            code = Internal;
            info =
              Some (Message "couldn't parse the gRPC message from the server");
          }
    | { status; _ } -> Error status
end

module Server_streaming = struct
  let call :
      channel:context Channel.t ->
      service:string ->
      method_name:string ->
      headers:Grpc_client.request_headers ->
      (Pbrt.Encoder.t -> unit) ->
      (Pbrt.Decoder.t -> unit) ->
      (unit, unit) result =
   fun ~channel ~service ~method_name ~headers encode_request read_handler ->
    let data_writer = single_write encode_request in
    let data_receiver = multi_read read_handler in

    let path = Grpc_client.make_path ~service ~method_name in
    let initial_context =
      { sent_all = false; read_state = Reading; parse_state = Idle }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | { status = { code = OK; _ }; _ } -> Ok ()
    | _ -> Error ()
end

module Client_streaming = struct
  let call :
      channel:context Channel.t ->
      service:string ->
      method_name:string ->
      headers:Grpc_client.request_headers ->
      (unit -> (Pbrt.Encoder.t -> unit) option) ->
      (Pbrt.Decoder.t, Status_new.t) result =
   fun ~(channel : context Channel.t) ~service ~method_name ~headers handler ->
    let encoder = Pbrt.Encoder.create ~size:1_000 () in
    let header_buffer = Cstruct.create 5 in

    let data_writer = multi_write ~encoder ~header_buffer handler in

    let data_receiver = single_read in

    let path = Grpc_client.make_path ~service ~method_name in
    let initial_context =
      { sent_all = false; read_state = Reading; parse_state = Idle }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | {
     status = { code = OK; _ };
     grpc_context = { read_state = Done decoder; _ };
    } ->
        Ok decoder
    | { status; _ } -> Error status
end

module Bidirectional_streaming = struct
  let call :
      channel:context Channel.t ->
      service:string ->
      method_name:string ->
      headers:Grpc_client.request_headers ->
      (unit -> (Pbrt.Encoder.t -> unit) option) ->
      (Pbrt.Decoder.t -> unit) ->
      (unit, unit) result =
   fun ~channel ~service ~method_name ~headers write_handler read_handler ->
    let encoder = Pbrt.Encoder.create ~size:1_000 () in
    let header_buffer = Cstruct.create 5 in
    let data_writer = multi_write ~encoder ~header_buffer write_handler in

    let data_receiver = multi_read read_handler in

    let path = Grpc_client.make_path ~service ~method_name in
    let initial_context =
      { sent_all = false; read_state = Reading; parse_state = Idle }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | { status = { code = OK; _ }; _ } -> Ok ()
    | _ -> Error ()
end
