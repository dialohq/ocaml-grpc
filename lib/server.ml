module GStatus = Status
open Haha
open Pbrt
open Utils

exception GrpcError of (GStatus.code * string)

type route_getter = service:string -> meth:string -> Reqd.handler_result option

let stream_error_handler : _ -> Error.t -> _ = fun c _code -> c
let grpc_headers = Header.of_list [ ("content-type", "application/grpc+proto") ]

let ok_trailers =
  Header.of_list [ ("grpc-status", "0"); ("grpc-message", "OK") ]

let not_found_trailers =
  Header.of_list
    [ ("grpc-status", "12"); ("grpc-message", "unimplemented service/method") ]

let internal_err_trailers exn =
  Header.of_list
    [
      ("grpc-status", "13");
      ( "grpc-message",
        Format.asprintf "internal server exception: %a" Eio.Exn.pp exn );
    ]

let custom_status_trailers : GStatus.code -> string -> Header.t list =
 fun code msg ->
  Header.of_list
    [
      ("grpc-status", GStatus.int_of_code code |> string_of_int);
      ("grpc-message", msg);
    ]

let catch_all f =
  try Ok (f ()) with
  | GrpcError (code, msg) -> Error (custom_status_trailers code msg)
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> Error (internal_err_trailers exn)

let encode_data ?header_buffer ?body_buffer encode_f encoder =
  Encoder.clear encoder;
  encode_f encoder;
  let length = Encoder.length encoder in

  let header_buffer = Option.value ~default:(Cstruct.create 5) header_buffer in
  fill_header ~length header_buffer;

  let body_buffer = Option.value ~default:(Cstruct.create length) body_buffer in
  Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
    body_buffer 0;

  [ header_buffer; Cstruct.sub body_buffer 0 length ]

let make_response_writer : 'c Body.writer -> 'c Response.response_writer =
 fun body_writer () ->
  `Final (Response.create_with_streaming ~body_writer `OK grpc_headers)

module Unary = struct
  type read_state = Reading | Done of Decoder.t
  type context = { read_state : read_state; parse_state : Body_parse.state }

  let body_writer : (Decoder.t -> single_writer) -> context Body.writer =
   fun get_encode_f ->
    let encoder = Encoder.create ~size:1_000 () in
    function
    | { read_state = Done decoder; _ } as context ->
        let payload =
          catch_all (fun () -> get_encode_f decoder)
          |> Result.fold
               ~ok:(fun x -> `End (Some (encode_data x encoder), ok_trailers))
               ~error:(fun x -> `End (None, x))
        in
        { payload; on_flush = ignore; context }
    | { read_state = Reading; _ } -> Eio.Fiber.await_cancel ()

  let body_reader : context Body.reader =
   fun context data ->
    match (data, context.read_state) with
    | `Data data, Reading ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in
        let read_state =
          match parsed with
          | [] -> Reading
          | x :: _ -> Done (Decoder.of_bytes x)
        in

        { read_state; parse_state }
    | _ -> context

  let respond : (Decoder.t -> single_writer) -> Reqd.handler_result =
   fun get_encoder ->
    let context = { read_state = Reading; parse_state = Idle } in
    let response_writer = make_response_writer (body_writer get_encoder) in
    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module ServerStreaming = struct
  type 'a read_state =
    | Reading of (Decoder.t -> 'a stream_writer)
    | Done of 'a stream_writer
    | Error of Header.t list

  type 'a context = {
    read_state : 'a read_state;
    parse_state : Body_parse.state;
    user_context : 'a;
  }

  let body_writer () : 'a context Body.writer =
    let encoder = Encoder.create ~size:1_000 () in
    let body_buffer = Cstruct.create 500_000 in
    let header_buffer = Cstruct.create 5 in
    function
    | { read_state = Error trailers; _ } as context ->
        { payload = `End (None, trailers); on_flush = ignore; context }
    | { read_state = Done writer; user_context; _ } as context ->
        let payload, user_context =
          catch_all (fun () ->
              let encode_opt, user_context = writer user_context in
              match encode_opt with
              | Some encode_f ->
                  ( `Data
                      (encode_data ~header_buffer ~body_buffer encode_f encoder),
                    user_context )
              | None -> (`End (None, ok_trailers), user_context))
          |> Result.fold
               ~ok:(fun x -> x)
               ~error:(fun x -> (`End (None, x), user_context))
        in
        { payload; on_flush = ignore; context = { context with user_context } }
    | { read_state = Reading _; _ } -> Eio.Fiber.await_cancel ()

  let body_reader : 'a context Body.reader =
   fun context data ->
    match (data, context.read_state) with
    | `Data data, Reading get_writer ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in
        let read_state : 'a read_state =
          match parsed with
          | [] -> Reading get_writer
          | x :: _ ->
              catch_all (fun () -> get_writer (Decoder.of_bytes x))
              |> Result.fold ~error:(fun h -> Error h) ~ok:(fun x -> Done x)
        in

        { context with read_state; parse_state }
    | _ -> context

  let respond : 'a -> (Decoder.t -> 'a stream_writer) -> Reqd.handler_result =
   fun context get_writer ->
    let context =
      {
        read_state = Reading get_writer;
        parse_state = Idle;
        user_context = context;
      }
    in
    let response_writer = make_response_writer (body_writer ()) in
    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module ClientStreaming = struct
  type 'a read_state = Reading | Done | Error of Header.t list

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
    | { read_state = Error trailers; _ } as context ->
        { payload = `End (None, trailers); on_flush = ignore; context }
    | { read_state = Done; respond; user_context; _ } as context ->
        let payload =
          catch_all (fun () -> respond user_context)
          |> Result.fold
               ~error:(fun x -> `End (None, x))
               ~ok:(fun x -> `End (Some (encode_data x encoder), ok_trailers))
        in
        { payload; on_flush = ignore; context }
    | { read_state = Reading; _ } -> Eio.Fiber.await_cancel ()

  let body_reader : _ context Body.reader =
   fun ({ user_context; reader; read_state; _ } as context) payload ->
    match payload with
    | `End _ ->
        let user_context, read_state =
          catch_all (fun () -> reader user_context None)
          |> Result.fold
               ~ok:(fun x -> (x, Done))
               ~error:(fun x -> (user_context, Error x))
        in
        { context with read_state; user_context }
    | `Data data ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in

        let user_context, read_state =
          catch_all (fun () ->
              List.fold_left
                (fun u_ctx b -> reader u_ctx (Some (Decoder.of_bytes b)))
                user_context parsed)
          |> Result.fold
               ~ok:(fun x -> (x, read_state))
               ~error:(fun x -> (user_context, Error x))
        in

        { context with parse_state; user_context; read_state }

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
    let response_writer = make_response_writer (body_writer ()) in
    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

module BidirectionalStreaming = struct
  type 'a context = {
    user_context : 'a;
    parse_state : Body_parse.state;
    reader : 'a stream_reader;
    writer : 'a stream_writer;
    errored : Header.t list option;
  }

  let body_writer () : _ context Body.writer =
    let encoder = Encoder.create ~size:1_000 () in
    let body_buffer = Cstruct.create 500_000 in
    let header_buffer = Cstruct.create 5 in
    function
    | { errored = Some trailers; _ } as context ->
        { payload = `End (None, trailers); on_flush = ignore; context }
    | { writer; user_context; _ } as context ->
        let payload, user_context =
          catch_all (fun () ->
              let encode_opt, user_context = writer user_context in
              match encode_opt with
              | Some encode_f ->
                  ( `Data
                      (encode_data ~header_buffer ~body_buffer encode_f encoder),
                    user_context )
              | None -> (`End (None, ok_trailers), user_context))
          |> Result.fold
               ~ok:(fun x -> x)
               ~error:(fun x -> (`End (None, x), user_context))
        in

        { payload; on_flush = ignore; context = { context with user_context } }

  let body_reader : _ context Body.reader =
   fun ({ user_context; reader; errored; _ } as context) payload ->
    match payload with
    | `End _ ->
        let user_context, errored =
          catch_all (fun () -> reader user_context None)
          |> Result.fold
               ~ok:(fun x -> (x, None))
               ~error:(fun x -> (user_context, Some x))
        in
        { context with errored; user_context }
    | `Data data ->
        let parse_state, parsed =
          Body_parse.read_messages data context.parse_state
        in

        let user_context, errored =
          catch_all (fun () ->
              List.fold_left
                (fun u_ctx b -> reader u_ctx (Some (Decoder.of_bytes b)))
                user_context parsed)
          |> Result.fold
               ~ok:(fun x -> (x, errored))
               ~error:(fun x -> (user_context, Some x))
        in

        { context with parse_state; errored; user_context }

  let respond :
      'a -> 'a stream_reader -> 'a stream_writer -> Reqd.handler_result =
   fun context reader writer ->
    let context =
      {
        user_context = context;
        parse_state = Idle;
        reader;
        writer;
        errored = None;
      }
    in
    let response_writer = make_response_writer (body_writer ()) in
    Reqd.handle ~context ~response_writer ~error_handler:stream_error_handler
      ~body_reader ()
end

let respond_not_found =
  Reqd.handle ~context:() ~body_reader:Body.ignore_reader
    ~error_handler:(fun c _ -> c)
    ~response_writer:(fun () -> `Final (Response.create `OK not_found_trailers))
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

  Server.connection_handler ~error_handler:ignore request_handler
