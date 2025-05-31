open Haha
open Pbrt

type single_writer = Encoder.t -> unit
type 'a stream_writer = 'a -> (Encoder.t -> unit) option * 'a
type 'a stream_reader = 'a -> Decoder.t option -> 'a
type route_getter = service:string -> meth:string -> Reqd.handler_result option

let error_handler : Error.connection_error -> unit = fun _error -> ()
let stream_error_handler : _ -> Error.t -> _ = fun c _code -> c
let grpc_headers = Header.of_list [ ("content-type", "application/grpc+proto") ]

let ok_trailers =
  Header.of_list [ ("grpc-status", "0"); ("grpc-message", "OK") ]

module Unary = struct
  type read_state = Reading | Done of Decoder.t
  type context = { read_state : read_state; parse_state : Body_parse.state }

  let body_writer : (Decoder.t -> Encoder.t -> unit) -> context Body.writer =
   fun get_encoder ->
    let encoder = Encoder.create ~size:1_000 () in
    fun (c as context) ->
      match c.read_state with
      | Done decoder ->
          Encoder.clear encoder;
          (get_encoder decoder) encoder;
          let length = Encoder.length encoder in

          let header_buffer = Cstruct.create 5 in
          Utils.fill_header_cs ~length header_buffer;

          let body_buffer = Cstruct.create length in
          Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
            encoder body_buffer 0;

          {
            payload = `End (Some [ header_buffer; body_buffer ], ok_trailers);
            on_flush = ignore;
            context;
          }
      | Reading -> Eio.Fiber.await_cancel ()

  let body_reader : context Body.reader =
   fun context data ->
    match (data, context.read_state) with
    | `Data data, Reading ->
        let new_parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in
        let read_state : read_state =
          match parsed with
          | [] -> Reading
          | x :: _ -> Done (Decoder.of_bytes x)
        in

        { read_state; parse_state = new_parse_state }
    | _ -> context

  let respond : (Decoder.t -> Encoder.t -> unit) -> Reqd.handler_result =
   fun get_encoder ->
    let context = { read_state = Reading; parse_state = Idle } in

    let response_writer : context Response.response_writer =
     fun () ->
      `Final
        (Response.create_with_streaming ~body_writer:(body_writer get_encoder)
           `OK grpc_headers)
    in

    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module ServerStreaming = struct
  type 'a read_state =
    | Reading of (Decoder.t -> 'a stream_writer)
    | Done of 'a stream_writer

  type 'a context = {
    read_state : 'a read_state;
    parse_state : Body_parse.state;
    user_context : 'a;
  }

  let body_writer () : 'a context Body.writer =
    let encoder = Encoder.create ~size:1_000 () in
    let body_buffer = Cstruct.create 500_000 in
    function
    | { read_state = Reading _; _ } -> Eio.Fiber.await_cancel ()
    | { read_state = Done writer; user_context; _ } as context -> (
        let encode_opt, user_context = writer user_context in

        match encode_opt with
        | Some encode ->
            Encoder.clear encoder;
            encode encoder;
            let length = Encoder.length encoder in

            let header_buffer = Cstruct.create 5 in
            Utils.fill_header_cs ~length header_buffer;

            Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
              encoder body_buffer 0;
            {
              payload =
                `Data [ header_buffer; Cstruct.sub body_buffer 0 length ];
              on_flush = ignore;
              context = { context with user_context };
            }
        | None ->
            {
              payload = `End (None, ok_trailers);
              on_flush = ignore;
              context = { context with user_context };
            })

  let body_reader : 'a context Body.reader =
   fun context data ->
    match (data, context.read_state) with
    | `Data data, Reading get_writer ->
        let new_parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in
        let read_state : 'a read_state =
          match parsed with
          | [] -> Reading get_writer
          | x :: _ -> Done (get_writer (Decoder.of_bytes x))
        in

        { context with read_state; parse_state = new_parse_state }
    | _ -> context

  let response_writer : _ context Response.response_writer =
   fun () ->
    `Final
      (Response.create_with_streaming ~body_writer:(body_writer ()) `OK
         grpc_headers)

  let respond : 'a -> (Decoder.t -> 'a stream_writer) -> Reqd.handler_result =
   fun context get_writer ->
    let context =
      {
        read_state = Reading get_writer;
        parse_state = Idle;
        user_context = context;
      }
    in

    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module ClientStreaming = struct
  type 'a read_state = Reading | Done

  type 'a context = {
    read_state : 'a read_state;
    reader : 'a stream_reader;
    respond : 'a -> single_writer;
    parse_state : Body_parse.state;
    user_context : 'a;
  }

  let body_writer : unit -> _ context Body.writer =
   fun () ->
    let encoder = Encoder.create ~size:1_000 () in
    function
    | { read_state = Reading; _ } -> Eio.Fiber.await_cancel ()
    | { read_state = Done; respond; user_context; _ } as context ->
        Encoder.clear encoder;
        (respond user_context) encoder;

        let length = Encoder.length encoder in

        let header_buffer = Cstruct.create 5 in
        Utils.fill_header_cs ~length header_buffer;

        let body_buffer = Cstruct.create length in
        Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
          body_buffer 0;

        {
          payload = `End (Some [ header_buffer; body_buffer ], ok_trailers);
          on_flush = ignore;
          context;
        }

  let body_reader : _ context Body.reader =
   fun ({ user_context; reader; _ } as context) payload ->
    match payload with
    | `End _ ->
        let user_context = reader user_context None in
        { context with read_state = Done; user_context }
    | `Data data ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in

        let user_context =
          List.fold_left
            (fun u_ctx b -> reader u_ctx (Some (Decoder.of_bytes b)))
            user_context parsed
        in

        { context with parse_state; user_context }

  let response_writer : _ context Response.response_writer =
   fun () ->
    `Final
      (Response.create_with_streaming ~body_writer:(body_writer ()) `OK
         grpc_headers)

  let respond :
      'a -> 'a stream_reader -> ('a -> single_writer) -> Reqd.handler_result =
   fun context reader respond ->
    let context =
      {
        user_context = context;
        read_state = Reading;
        reader;
        respond;
        parse_state = Idle;
      }
    in

    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module BidirectionalStreaming = struct
  type 'a context = {
    user_context : 'a;
    parse_state : Body_parse.state;
    reader : 'a stream_reader;
    writer : 'a stream_writer;
  }

  let body_writer () : _ context Body.writer =
    let encoder = Encoder.create ~size:1_000 () in
    let body_buffer = Cstruct.create 500_000 in
    fun ({ writer; user_context; _ } as context) ->
      let encode_opt, user_context = writer user_context in

      match encode_opt with
      | Some encode ->
          Encoder.clear encoder;
          encode encoder;
          let length = Encoder.length encoder in

          let header_buffer = Cstruct.create 5 in
          Utils.fill_header_cs ~length header_buffer;

          Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
            encoder body_buffer 0;
          {
            payload = `Data [ header_buffer; Cstruct.sub body_buffer 0 length ];
            on_flush = ignore;
            context = { context with user_context };
          }
      | None ->
          {
            payload = `End (None, ok_trailers);
            on_flush = ignore;
            context = { context with user_context };
          }

  let body_reader : _ context Body.reader =
   fun ({ user_context; reader; _ } as context) payload ->
    match payload with
    | `End _ ->
        let user_context = reader user_context None in
        { context with user_context }
    | `Data data ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in

        let user_context =
          List.fold_left
            (fun u_ctx b -> reader u_ctx (Some (Decoder.of_bytes b)))
            user_context parsed
        in

        { context with parse_state; user_context }

  let response_writer : _ context Response.response_writer =
   fun () ->
    `Final
      (Response.create_with_streaming ~body_writer:(body_writer ()) `OK
         grpc_headers)

  let respond :
      'a -> 'a stream_reader -> 'a stream_writer -> Reqd.handler_result =
   fun context reader writer ->
    let context =
      { user_context = context; parse_state = Idle; reader; writer }
    in
    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

let respond_not_found =
  Reqd.handle ~context:() ~body_reader:Body.ignore_reader
    ~error_handler:(fun c _ -> c)
    ~response_writer:(fun () ->
      `Final
        (Response.create `OK
           (Header.of_list
              [
                ("grpc-status", "12");
                ("grpc-message", "unimplemented service/method");
              ])))
    ()

let connection_handler : route_getter -> _ Eio.Net.connection_handler =
 fun get_route ->
  let request_handler : Reqd.handler =
   fun reqd ->
    let path, _headers = Reqd.(path reqd, headers reqd) in

    match String.split_on_char '/' path with
    | [ ""; service; meth ] ->
        Option.value ~default:respond_not_found (get_route ~service ~meth)
    | _ -> respond_not_found
  in

  Server.connection_handler ~error_handler request_handler
