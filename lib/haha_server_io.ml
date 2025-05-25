exception Unexpected_eof
exception Connection_error of Haha.Error.connection_error

let wrap_in_promise f =
  let p, r = Eio.Promise.create () in
  f (fun () -> Eio.Promise.resolve r ());
  Eio.Promise.await p

type 'context net_request = {
  request : Haha.Reqd.t;
  msg_stream : Body_parse.t Body_parse.consumer option Eio.Stream.t;
  handler_resolver : 'context Haha.Reqd.handler_result Eio.Promise.u;
  connection_error : Haha.Error.connection_error Eio.Promise.t;
  buffer_pool : Buffer_pool.Bytes_pool.t;
}

module Io = struct
  type request = Pbrt.Decoder.t Legacy_modules.Body_reader.consumer
  type response = Pbrt.Encoder.t -> unit

  module Net_request = struct
    type t = unit net_request

    let to_seq recv =
      let rec loop recv () =
        match recv () with
        | Legacy_modules.Recv_seq.Done -> Seq.Nil
        | Next (x, recv) -> Seq.Cons (x, loop recv)
        | Err `Unexpected_eof -> raise Unexpected_eof
        | Err (`Connection_error error) -> raise (Connection_error error)
      in
      loop recv

    let body ({ msg_stream; _ } : t) : request Seq.t =
      let rec get_next () =
        match Eio.Stream.take msg_stream with
        | Some msg -> Legacy_modules.Recv_seq.Next (msg, fun () -> get_next ())
        | None -> Done
      in

      (fun () -> get_next ())
      |> Legacy_modules.Recv_seq.map (fun { Body_parse.consume } ->
             {
               Legacy_modules.Body_reader.consume =
                 (fun f ->
                   consume (fun { Body_parse.bytes; len } ->
                       f (Pbrt.Decoder.of_subbytes bytes 0 len)));
             })
      |> to_seq

    let is_post ({ request; _ } : t) = Haha.Reqd.meth request = POST
    let target ({ request; _ } : t) = Haha.Reqd.path request

    let get_header ({ request; _ } : t) s =
      Haha.Header.find_opt s @@ Haha.Reqd.headers request
  end

  let grpc_trailers_to_headers
      ({ grpc_status; grpc_message; extra } :
        Legacy_modules.Grpc_server.trailers) =
    Haha.Header.of_list
      (("grpc-status", string_of_int grpc_status)
      ::
      (match grpc_message with
      | None -> extra
      | Some msg -> ("grpc-message", msg) :: extra))

  let respond_streaming ~(headers : Legacy_modules.Grpc_server.headers)
      ({ msg_stream; handler_resolver; buffer_pool = pool; _ } : Net_request.t)
      : response Legacy_modules.Io.streaming_writer =
    let open Haha in
    let body_writer_stream = Eio.Stream.create 0 in
    let write_body cs =
      wrap_in_promise @@ fun resolve ->
      Eio.Stream.add body_writer_stream (`Data cs, resolve)
    in
    let body_writer () ~window_size:_ =
      let payload, on_flush = Eio.Stream.take body_writer_stream in

      { Types.payload; on_flush; context = () }
    in
    let write_end ?(trailers = []) () =
      wrap_in_promise (fun resolve ->
          Eio.Stream.add body_writer_stream (`End (None, trailers), resolve))
    in

    let msg_state = ref Body_parse.Idle in

    let on_data () = function
      | `Data data ->
          let new_state, parsed = Body_parse.read_messages data !msg_state in
          List.iter
            (fun b ->
              Eio.Stream.add msg_stream
                (Some
                   (Body_parse.to_consumer ~pool
                      { bytes = b; len = Bytes.length b })))
            parsed;
          msg_state := new_state;
          { Types.action = `Continue; context = () }
      | `End _ ->
          Eio.Stream.add msg_stream None;
          { Types.action = `Continue; context = () }
    in

    let error_handler =
     fun () code ->
      Format.printf "[GRPC_IO] H2 stream error: %a@." Haha.Error_code.pp_hum
        code
    in

    let handler =
      {
        Reqd.initial_context = ();
        error_handler;
        on_data;
        response_writer =
          (fun () ->
            `Final
              (Response.create_with_streaming ~body_writer `OK
                 (Header.of_list
                    (( "content-type",
                       headers.Legacy_modules.Grpc_server.content_type )
                    :: headers.extra))));
      }
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

    let write_trailers (trailers : Legacy_modules.Grpc_server.trailers) =
      write_end ~trailers:(grpc_trailers_to_headers trailers) ()
    in

    let is_closed () = false in

    Eio.Promise.resolve handler_resolver handler;
    {
      Legacy_modules.Grpc_server_eio.Io.write;
      close;
      write_trailers;
      is_closed;
    }

  let respond_error ~(status_code : int) ~(headers : (string * string) list)
      ({ handler_resolver; _ } : Net_request.t) : unit =
    let open Haha in
    let error_handler =
     fun () code ->
      Format.printf "[GRPC_IO] H2 stream error: %a@." Haha.Error_code.pp_hum
        code
    in
    let handler =
      {
        Reqd.initial_context = ();
        error_handler;
        on_data = (fun _ _ -> { Types.action = `Continue; context = () });
        response_writer =
          (fun () ->
            `Final
              (Response.create
                 (Status.of_code status_code)
                 (Header.of_list headers)));
      }
    in

    Eio.Promise.resolve handler_resolver handler
end

include Io

let io =
  (module Io : Legacy_modules.Io.S
    with type Net_request.t = Io.Net_request.t
     and type request = Pbrt.Decoder.t Legacy_modules.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit)

let connection_handler ~sw ?debug:_ ?config ?grpc_error_handler server =
  let error_p, error_r = Eio.Promise.create () in
  let buffer_pool = Buffer_pool.Bytes_pool.make () in

  let request_handler (request : Haha.Reqd.t) =
    let handler_promise, handler_resolver = Eio.Promise.create () in
    let msg_stream = Eio.Stream.create Int.max_int in

    Eio.Fiber.fork ~sw (fun () ->
        Legacy_modules.Grpc_server_eio.handle_request ~io
          ?error_handler:grpc_error_handler server
          {
            request;
            msg_stream;
            handler_resolver;
            connection_error = error_p;
            buffer_pool;
          });

    Eio.Promise.await handler_promise
  in

  let error_handler = function
    | (Haha.Error.ProtocolViolation (code, msg) | PeerError (code, msg)) as err
      ->
        Format.printf "[GPRC_IO] H2 protocol error: %a, %s@."
          Haha.Error_code.pp_hum code msg;
        Eio.Promise.try_resolve error_r err |> ignore
    | Exn exn as err ->
        Printf.printf "[GRPC_IO] H2 exception: %s\n%!" @@ Printexc.to_string exn;
        Eio.Promise.try_resolve error_r err |> ignore
  in

  Haha.Server.connection_handler ~error_handler ?config request_handler
