open Pbrt
open Utils

(* functional style readers/writers *)
type read_state = Reading | Done of Decoder.t
type 'a writer = Single of single_writer | Stream of 'a stream_writer
type 'a reader = Single | Stream of 'a stream_reader

(* imperative style readers/writers *)
type imp_writer = (Encoder.t -> unit) option -> unit
type imp_reader = Decoder.t Seq.t
type 'a reading_handler = reader:imp_reader -> 'a
type 'a writing_handler = writer:imp_writer -> 'a
type 'a bi_handler = writer:imp_writer -> reader:imp_reader -> 'a

type 'a imp_handler =
  | Reading of ('a reading_handler * single_writer)
  | Writing of 'a writing_handler
  | Bi of 'a bi_handler

type 'c context = {
  sent_all : bool;
  read_state : read_state;
  parse_state : Body_parse.state;
  on_data : 'c -> Decoder.t option -> 'c;
  write_data : 'c -> (Encoder.t -> unit) option * 'c;
  context : 'c;
}

let single_write : single_writer -> _ context Channel.data_writer =
 fun encode_request ->
  let encoder = Encoder.create ~size:1_000 () in
  Encoder.clear encoder;
  encode_request encoder;
  let length = Encoder.length encoder in

  let header_buffer = Cstruct.create 5 in
  fill_header ~length header_buffer;

  let body_buffer = Cstruct.create length in
  Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
    body_buffer 0;

  fun context ->
    if context.sent_all then (None, context)
    else (Some [ header_buffer; body_buffer ], { context with sent_all = true })

let single_read : _ context Channel.data_receiver =
 fun (c as context) data ->
  match (data, c.read_state) with
  | Some data, Reading ->
      let new_parse_state, parsed =
        Body_parse.read_messages data c.parse_state
      in
      let read_state : read_state =
        match parsed with [] -> Reading | x :: _ -> Done (Decoder.of_bytes x)
      in
      { c with read_state; parse_state = new_parse_state }
  | _ -> context

let multi_write :
    encoder:Encoder.t ->
    header_buffer:Cstruct.t ->
    body_buffer:Cstruct.t ->
    _ context Channel.data_writer =
 fun ~encoder ~header_buffer ~body_buffer c ->
  match c.write_data c.context with
  | None, context -> (None, { c with sent_all = true; context })
  | Some encode_request, context ->
      Encoder.clear encoder;
      encode_request encoder;
      let length = Encoder.length encoder in

      fill_header ~length header_buffer;

      Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes encoder
        body_buffer 0;
      ( Some [ header_buffer; Cstruct.sub body_buffer 0 length ],
        { c with context } )

let multi_read : _ context Channel.data_receiver =
 fun c -> function
  | Some data ->
      let parse_state, parsed = Body_parse.read_messages data c.parse_state in

      let new_context =
        List.fold_left
          (fun acc msg -> c.on_data acc (Some (Decoder.of_bytes msg)))
          c.context parsed
      in

      { c with parse_state; context = new_context }
  | None ->
      let new_context = c.on_data c.context None in

      { c with context = new_context }

let make_imp_writer () : imp_writer * unit stream_writer =
  let write_stream = Eio.Stream.create max_int in
  let write_data : unit stream_writer =
   fun () -> (Eio.Stream.take write_stream, ())
  in
  let writer : (Encoder.t -> unit) option -> unit =
   fun encode_request -> Eio.Stream.add write_stream encode_request
  in
  (writer, write_data)

let make_imp_reader () : imp_reader * unit stream_reader =
  let read_stream = Eio.Stream.create max_int in
  let on_data : unit stream_reader =
   fun () decoder -> Eio.Stream.add read_stream decoder
  in
  let rec reader : Decoder.t Seq.t =
   fun () ->
    match Eio.Stream.take read_stream with
    | None -> Seq.Nil
    | Some decoder -> Cons (decoder, reader)
  in
  (reader, on_data)

let call_gen :
    channel:Channel.t ->
    initial_context:'a ->
    service:string ->
    method_name:string ->
    'a writer ->
    'a reader ->
    'a context Channel.stream_result Eio.Promise.t =
 fun ~channel ~initial_context ~service ~method_name writer reader ->
  let data_writer, write_data =
    match writer with
    | Single encode_request -> (single_write encode_request, Obj.magic ())
    | Stream handler ->
        let encoder = Encoder.create ~size:1_000 () in
        let header_buffer = Cstruct.create 5 in
        let body_buffer = Cstruct.create 500_000 in
        (multi_write ~encoder ~header_buffer ~body_buffer, handler)
  in

  let data_receiver, on_data =
    match reader with
    | Single -> (single_read, Obj.magic ())
    | Stream handler -> (multi_read, handler)
  in

  let initial_context =
    {
      sent_all = false;
      read_state = Reading;
      parse_state = Idle;
      context = initial_context;
      on_data;
      write_data;
    }
  in

  let path = Printf.sprintf "/%s/%s" service method_name in
  let headers =
    [ ("te", "trailers"); ("content-type", "application/grpc+proto") ]
  in
  Channel.start_request ~headers ~data_writer ~data_receiver ~path
    ~initial_context channel

let imperative_call_gen :
    sw:Eio.Switch.t ->
    channel:Channel.t ->
    service:string ->
    method_name:string ->
    'a imp_handler ->
    unit context Channel.stream_result Eio.Promise.t * 'a Eio.Promise.t =
 fun ~sw ~channel ~service ~method_name handler ->
  let call_gen = call_gen ~channel ~initial_context:() ~service ~method_name in
  match handler with
  | Bi bi_handler ->
      let writer, write_data = make_imp_writer () in
      let reader, on_data = make_imp_reader () in
      let handler () = bi_handler ~writer ~reader in

      let handler_result_p, handler_result_r = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Promise.resolve handler_result_r @@ handler ();
          `Stop_daemon);

      (call_gen (Stream write_data) (Stream on_data), handler_result_p)
  | Reading (read_handler, encode_request) ->
      let reader, on_data = make_imp_reader () in
      let handler () = read_handler ~reader in

      let handler_result_p, handler_result_r = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Promise.resolve handler_result_r @@ handler ();
          `Stop_daemon);

      (call_gen (Single encode_request) (Stream on_data), handler_result_p)
  | Writing write_handler ->
      let writer, write_data = make_imp_writer () in
      let handler () = write_handler ~writer in

      let handler_result_p, handler_result_r = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Promise.resolve handler_result_r @@ handler ();
          `Stop_daemon);

      (call_gen (Stream write_data) Single, handler_result_p)

module Unary = struct
  let call :
      channel:Channel.t ->
      service:string ->
      method_name:string ->
      (Encoder.t -> unit) ->
      (Decoder.t, Status.t) result =
   fun ~channel ~service ~method_name encode_request ->
    match
      Eio.Promise.await
      @@ call_gen ~initial_context:(Obj.magic ()) ~channel ~service ~method_name
           (Single encode_request) Single
    with
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

module ServerStreaming = struct
  module Expert = struct
    let call :
        channel:Channel.t ->
        initial_context:'a ->
        service:string ->
        method_name:string ->
        (Encoder.t -> unit) ->
        'a stream_reader ->
        ('a, Status.t) result =
     fun ~channel ~initial_context ~service ~method_name encode_request
         read_handler ->
      match
        Eio.Promise.await
        @@ call_gen ~channel ~initial_context ~service ~method_name
             (Single encode_request) (Stream read_handler)
      with
      | { status = { code = OK; _ }; grpc_context = { context; _ } } ->
          Ok context
      | { status; _ } -> Error status
  end

  let call :
      channel:Channel.t ->
      service:string ->
      method_name:string ->
      (Encoder.t -> unit) ->
      'a reading_handler ->
      ('a, Status.t) result =
   fun ~channel ~service ~method_name encode_request handler ->
    Eio.Switch.run @@ fun sw ->
    let status_promise, handler_promise =
      imperative_call_gen ~sw ~channel ~service ~method_name
        (Reading (handler, encode_request))
    in
    match Eio.Promise.await status_promise with
    | { status = { code = OK; _ }; _ } -> Ok (Eio.Promise.await handler_promise)
    | { status; _ } -> Error status
end

module ClientStreaming = struct
  module Expert = struct
    let call :
        channel:Channel.t ->
        initial_context:'a ->
        service:string ->
        method_name:string ->
        'a stream_writer ->
        (Decoder.t * 'a, Status.t) result =
     fun ~channel ~initial_context ~service ~method_name handler ->
      match
        Eio.Promise.await
        @@ call_gen ~channel ~initial_context ~service ~method_name
             (Stream handler) Single
      with
      | {
       status = { code = OK; _ };
       grpc_context = { read_state = Done decoder; context; _ };
      } ->
          Ok (decoder, context)
      | { status; _ } -> Error status
  end

  let call :
      channel:Channel.t ->
      service:string ->
      method_name:string ->
      'a writing_handler ->
      (Decoder.t * 'a, Status.t) result =
   fun ~channel ~service ~method_name handler ->
    Eio.Switch.run @@ fun sw ->
    let status_promise, handler_promise =
      imperative_call_gen ~sw ~channel ~service ~method_name (Writing handler)
    in
    match Eio.Promise.await status_promise with
    | {
     status = { code = OK; _ };
     grpc_context = { read_state = Done decoder; _ };
    } ->
        Ok (decoder, Eio.Promise.await handler_promise)
    | { status; _ } -> Error status
end

module BidirectionalStreaming = struct
  module Expert = struct
    let call :
        channel:Channel.t ->
        initial_context:'a ->
        service:string ->
        method_name:string ->
        'a stream_writer ->
        'a stream_reader ->
        ('a, Status.t) result =
     fun ~channel ~initial_context ~service ~method_name write_handler
         read_handler ->
      match
        Eio.Promise.await
        @@ call_gen ~channel ~initial_context ~service ~method_name
             (Stream write_handler) (Stream read_handler)
      with
      | { status = { code = OK; _ }; grpc_context = { context; _ } } ->
          Ok context
      | { status; _ } -> Error status
  end

  let call :
      channel:Channel.t ->
      service:string ->
      method_name:string ->
      'a bi_handler ->
      ('a, Status.t) result =
   fun ~channel ~service ~method_name handler ->
    Eio.Switch.run @@ fun sw ->
    let status_promise, handler_promise =
      imperative_call_gen ~sw ~channel ~service ~method_name (Bi handler)
    in
    match Eio.Promise.await status_promise with
    | { status = { code = OK; _ }; _ } -> Ok (Eio.Promise.await handler_promise)
    | { status; _ } -> Error status
end
