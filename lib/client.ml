type read_state = Reading | Done of Pbrt.Decoder.t

type context =
  | Context : {
      sent_all : bool;
      read_state : read_state;
      parse_state : Body_parse.state;
      read_handler : 'c -> Pbrt.Decoder.t -> 'c;
      write_handler : 'c -> (Pbrt.Encoder.t -> unit) option * 'c;
      context : 'c;
    }
      -> context

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

  fun (Context context) ->
    if context.sent_all then (None, Context context)
    else
      ( Some [ header_buffer; body_buffer ],
        Context { context with sent_all = true } )

let single_read : context Channel.data_receiver =
 fun (Context c as context) data ->
  match c.read_state with
  | Done _ -> { action = `Reset; context }
  | Reading ->
      let new_parse_state, parsed =
        Body_parse.read_messages data c.parse_state
      in
      let read_state =
        match parsed with
        | [] -> Reading
        | x :: _ -> Done (Pbrt.Decoder.of_bytes x)
      in
      {
        action = `Continue;
        context = Context { c with read_state; parse_state = new_parse_state };
      }

let multi_write ~encoder ~header_buffer : context Channel.data_writer =
 fun (Context c) ->
  match c.write_handler c.context with
  | None, context -> (None, Context { c with sent_all = true; context })
  | Some encode_request, context ->
      Pbrt.Encoder.clear encoder;
      encode_request encoder;
      let length = Pbrt.Encoder.length encoder in

      fill_header_cs ~length header_buffer;

      let body_buffer = Cstruct.create length in
      Pbrt.Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
        encoder body_buffer 0;
      (Some [ header_buffer; body_buffer ], Context { c with context })

let multi_read : context Channel.data_receiver =
 fun (Context c) data ->
  let parse_state, parsed = Body_parse.read_messages data c.parse_state in

  let new_context =
    List.fold_left
      (fun acc msg -> c.read_handler acc (Pbrt.Decoder.of_bytes msg))
      c.context parsed
  in

  {
    action = `Continue;
    context = Context { c with parse_state; context = new_context };
  }

module Unary = struct
  let call :
      channel:context Channel.t ->
      service:string ->
      method_name:string ->
      headers:Legacy_modules.Grpc_client.request_headers ->
      (Pbrt.Encoder.t -> unit) ->
      (Pbrt.Decoder.t, Status.t) result =
   fun ~channel ~service ~method_name ~headers encode_request ->
    let data_writer = single_write encode_request in

    let data_receiver = single_read in

    let path = Legacy_modules.Grpc_client.make_path ~service ~method_name in
    let initial_context =
      Context
        {
          sent_all = false;
          read_state = Reading;
          parse_state = Idle;
          context = Obj.magic ();
          read_handler = Obj.magic ();
          write_handler = Obj.magic ();
        }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | {
     status = { code = OK; _ };
     grpc_context = Context { read_state = Done decoder; _ };
    } ->
        Ok decoder
    | {
     status = { code = OK; _ };
     grpc_context = Context { read_state = Reading; _ };
    } ->
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
      initial_context:'a ->
      service:string ->
      method_name:string ->
      headers:Legacy_modules.Grpc_client.request_headers ->
      (Pbrt.Encoder.t -> unit) ->
      ('a -> Pbrt.Decoder.t -> 'a) ->
      (unit, unit) result =
   fun ~channel ~initial_context ~service ~method_name ~headers encode_request
       read_handler ->
    let data_writer = single_write encode_request in

    let path = Legacy_modules.Grpc_client.make_path ~service ~method_name in
    let initial_context =
      Context
        {
          sent_all = false;
          read_state = Reading;
          parse_state = Idle;
          context = initial_context;
          read_handler;
          write_handler = Obj.magic ();
        }
    in
    let data_receiver = multi_read in
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
      initial_context:'a ->
      service:string ->
      method_name:string ->
      headers:Legacy_modules.Grpc_client.request_headers ->
      ('a -> (Pbrt.Encoder.t -> unit) option * 'a) ->
      (Pbrt.Decoder.t, Status.t) result =
   fun ~channel ~initial_context ~service ~method_name ~headers handler ->
    let encoder = Pbrt.Encoder.create ~size:1_000 () in
    let header_buffer = Cstruct.create 5 in

    let data_writer = multi_write ~encoder ~header_buffer in

    let data_receiver = single_read in

    let path = Legacy_modules.Grpc_client.make_path ~service ~method_name in
    let initial_context =
      Context
        {
          sent_all = false;
          read_state = Reading;
          parse_state = Idle;
          context = initial_context;
          read_handler = Obj.magic ();
          write_handler = handler;
        }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | {
     status = { code = OK; _ };
     grpc_context = Context { read_state = Done decoder; _ };
    } ->
        Ok decoder
    | { status; _ } -> Error status
end

module Bidirectional_streaming = struct
  let call :
      channel:context Channel.t ->
      initial_context:'a ->
      service:string ->
      method_name:string ->
      headers:Legacy_modules.Grpc_client.request_headers ->
      ('a -> (Pbrt.Encoder.t -> unit) option * 'a) ->
      ('a -> Pbrt.Decoder.t -> 'a) ->
      (unit, unit) result =
   fun ~channel ~initial_context ~service ~method_name ~headers write_handler
       read_handler ->
    let encoder = Pbrt.Encoder.create ~size:1_000 () in
    let header_buffer = Cstruct.create 5 in
    let data_writer = multi_write ~encoder ~header_buffer in

    let data_receiver = multi_read in

    let path = Legacy_modules.Grpc_client.make_path ~service ~method_name in
    let initial_context =
      Context
        {
          sent_all = false;
          read_state = Reading;
          parse_state = Idle;
          context = initial_context;
          read_handler;
          write_handler;
        }
    in
    let status_promise =
      Channel.start_request ~headers ~data_writer ~data_receiver ~path
        ~initial_context channel
    in

    match Eio.Promise.await status_promise with
    | { status = { code = OK; _ }; _ } -> Ok ()
    | _ -> Error ()
end
