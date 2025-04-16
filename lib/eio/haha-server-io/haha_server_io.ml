exception Unexpected_eof
exception Connection_error of Haha.Error.connection_error

let wrap_in_promise f =
  let p, r = Eio.Promise.create () in
  f (fun () -> Eio.Promise.resolve r ());
  Eio.Promise.await p

module Io = struct
  type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
  type response = Pbrt.Encoder.t -> unit

  module Net_request = struct
    type t =
      Haha.Request.t
      * Grpc_eio_core.Body_parse.t Grpc_eio_core.Body_parse.consumer option
        Eio.Stream.t
      * (Haha.Types.body_reader * Haha.Response.response_writer) Eio.Promise.u
      * Haha.Error.connection_error Eio.Promise.t

    let to_seq recv =
      let rec loop recv () =
        match recv () with
        | Grpc_eio_core.Recv_seq.Done -> Seq.Nil
        | Next (x, recv) -> Seq.Cons (x, loop recv)
        | Err `Unexpected_eof -> raise Unexpected_eof
        | Err (`Connection_error error) -> raise (Connection_error error)
      in
      loop recv

    let body ((_, msg_stream, _, _err_p) : t) : request Seq.t =
      let rec get_next () =
        match Eio.Stream.take msg_stream with
        | Some msg -> Grpc_eio_core.Recv_seq.Next (msg, fun () -> get_next ())
        | None -> Done
      in

      (fun () -> get_next ())
      |> Grpc_eio_core.Recv_seq.map (fun { Grpc_eio_core.Body_parse.consume } ->
             {
               Grpc_eio_core.Body_reader.consume =
                 (fun f ->
                   consume (fun { Grpc_eio_core.Body_parse.bytes; len } ->
                       f (Pbrt.Decoder.of_subbytes bytes 0 len)));
             })
      |> to_seq

    let is_post ((req, _, _, _) : t) = Haha.Request.meth req = POST
    let target ((req, _, _, _) : t) = Haha.Request.path req

    let get_header ((req, _, _, _) : t) =
      Haha.Headers.get_string @@ Haha.Request.headers req
  end

  let grpc_trailers_to_headers
      ({ grpc_status; grpc_message; extra } : Grpc_server.trailers) =
    Haha.Headers.of_list
      (("grpc-status", string_of_int grpc_status)
      ::
      (match grpc_message with
      | None -> extra
      | Some msg -> ("grpc-message", msg) :: extra))

  let respond_streaming ~(headers : Grpc_server.headers)
      ((_, msg_stream, handler_resolver, _) : Net_request.t) :
      response Grpc_server_eio.Io.streaming_writer =
    let open Haha in
    let body_writer_stream = Eio.Stream.create 0 in
    let write_body cs =
      wrap_in_promise @@ fun resolve ->
      Eio.Stream.add body_writer_stream (`Data cs, resolve)
    in
    let body_writer ~window_size:_ = Eio.Stream.take body_writer_stream in
    let write_end ?(trailers = []) () =
      wrap_in_promise (fun resolve ->
          Eio.Stream.add body_writer_stream (`End (None, trailers), resolve))
    in

    let msg_state = ref Grpc_eio_core.Body_parse.Idle in

    let on_data = function
      | `Data { Cstruct.buffer = data; len; off } ->
          Grpc_eio_core.Body_parse.read_message ~data ~len ~off msg_stream
            msg_state
      | `End _ -> Eio.Stream.add msg_stream None
    in

    let handler =
      Request.handle ~on_data ~response_writer:(fun () ->
          `Final
            (Response.create_with_streaming ~body_writer `OK
               (Headers.of_list
                  (("content-type", headers.Grpc_server.content_type)
                  :: headers.extra))))
    in

    let fill_header_cs ~length (buffer : Cstruct.t) =
      (* write compressed flag (uint8) *)
      Cstruct.set_char buffer 0 '\x00';
      (* write msg length (uint32 be) *)
      Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
      Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)
    in

    let encoder = Pbrt.Encoder.create () in
    let header_buffer = Cstruct.create 5 in
    let body_buffer = Cstruct.create 400_000 in
    let write (input : response) =
      Pbrt.Encoder.clear encoder;
      input encoder;
      let data = Pbrt.Encoder.to_bytes encoder in
      let length = Bytes.length data in
      fill_header_cs ~length header_buffer;

      Cstruct.blit_from_bytes data 0 body_buffer 0 length;
      write_body [ header_buffer; Cstruct.sub body_buffer 0 length ]
    in

    let close () = write_end () in

    let write_trailers (trailers : Grpc_server.trailers) =
      write_end ~trailers:(grpc_trailers_to_headers trailers) ()
    in

    let is_closed () = false in

    Eio.Promise.resolve handler_resolver handler;
    { Grpc_server_eio.Io.write; close; write_trailers; is_closed }

  let respond_error ~(status_code : int) ~(headers : (string * string) list)
      ((_, _, handler_resolver, _) : Net_request.t) : unit =
    let open Haha in
    let handler =
      Request.handle ~on_data:ignore ~response_writer:(fun () ->
          `Final
            (Response.create
               (Status.of_code status_code)
               (Headers.of_list headers)))
    in

    Eio.Promise.resolve handler_resolver handler
end

include Io

let io =
  (module Io : Grpc_server_eio.Io.S
    with type Net_request.t = Io.Net_request.t
     and type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit)

let connection_handler ~sw ?debug ?config ?h2_error_handler ?grpc_error_handler
    server =
  let error_p, error_r = Eio.Promise.create () in

  let request_handler (request : Haha.Request.t) =
    let handler_promise, handler_resolver = Eio.Promise.create () in
    let msg_stream = Eio.Stream.create Int.max_int in

    Eio.Fiber.fork ~sw (fun () ->
        Grpc_server_eio.handle_request ~io ?error_handler:grpc_error_handler
          server
          (request, msg_stream, handler_resolver, error_p));

    Eio.Promise.await handler_promise
  in

  Haha.Server.connection_handler ?debug
    ~error_handler:(fun err ->
      (match err with
      | ConnectionError ((code, msg) as conn_err) ->
          Format.printf "Server connection error: %a, %s@."
            Haha.Error_code.pp_hum code msg;
          Eio.Promise.resolve error_r conn_err
      | StreamError (id, code) ->
          Format.printf "Server stream error on id %li: %a@." id
            Haha.Error_code.pp_hum code);
      match h2_error_handler with None -> () | Some handler -> handler err)
    (Option.value ~default:Haha.Settings.default config)
    request_handler
