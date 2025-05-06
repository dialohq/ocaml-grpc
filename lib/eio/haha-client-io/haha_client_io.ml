type stream_error = [ `Unexpected_eof ]

type t =
  ( Haha.Headers.t list,
    Haha.Response.t,
    Pbrt.Encoder.t -> unit,
    Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
    stream_error,
    Haha.Error.connection_error )
  Grpc_client_eio.Io.t

module type Connection = sig
  val write_request : Haha.Request.t -> unit
  val connection_error : Haha.Error.connection_error Eio.Promise.t
  val scheme : string
  val host : string
  val debug : bool
  val buffer_pool : Grpc_eio_core.Buffer_pool.Bytes_pool.t
end

let wrap_in_promise _ f =
  let p, r = Eio.Promise.create () in
  f (fun () -> Eio.Promise.resolve r ());
  Eio.Promise.await p

module MakeHahaIO (Connection : Connection) :
  Grpc_client_eio.Io.S
    with type Net_response.t = Haha.Response.t
     and type Headers.t = Haha.Headers.t list
     and type connection_error = Haha.Error.connection_error
     and type request = Pbrt.Encoder.t -> unit
     and type response = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type stream_error = stream_error = struct
  module Headers = struct
    type t = Haha.Headers.t list

    let get = Haha.Headers.get_string
  end

  module Net_response = struct
    type t = Haha.Response.t

    let is_ok t = Haha.Response.status t = `OK
    let headers t = Haha.Response.headers t
  end

  type connection_error = Haha.Error.connection_error
  type nonrec stream_error = stream_error
  type request = Pbrt.Encoder.t -> unit
  type response = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer

  type client_error =
    | Unary of
        ( response,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t )
        Grpc_client_eio.Rpc_error.Unary.error'
    | Client_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t,
          response )
        Grpc_client_eio.Rpc_error.Client_streaming.error'
        -> client_error
    | Server_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          Net_response.t,
          connection_error )
        Grpc_client_eio.Rpc_error.Server_streaming.error'
        -> client_error
    | Bidirectional_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t )
        Grpc_client_eio.Rpc_error.Bidirectional_streaming.error'
        -> client_error

  exception Grpc_client_error of client_error

  let raise_client_error (error : client_error) =
    raise (Grpc_client_error error)

  let send_request ~(headers : Grpc_client.request_headers) (path : string) =
    let body_writer_stream = Eio.Stream.create Int.max_int in
    let body_writer ~window_size:_ = Eio.Stream.take body_writer_stream in
    let write_body cs =
      wrap_in_promise Connection.debug @@ fun resolve ->
      Eio.Stream.add body_writer_stream (`Data cs, resolve)
    in
    let close_writer () =
      wrap_in_promise Connection.debug @@ fun resolve ->
      Eio.Stream.add body_writer_stream (`End (None, []), resolve)
    in

    let trailers_t, trailers_u = Eio.Promise.create () in

    let msg_state = ref Grpc_eio_core.Body_parse.Idle in
    let msg_stream = Eio.Stream.create Int.max_int in

    let on_data = function
      | `Data { Cstruct.buffer = data; len; off } ->
          Grpc_eio_core.Body_parse.read_message ~pool:Connection.buffer_pool
            ~data ~len ~off msg_stream msg_state
      | `End (None, trailers) ->
          Eio.Stream.add msg_stream None;
          Eio.Promise.resolve trailers_u trailers
      | `End (Some { Cstruct.buffer = data; len; off }, trailers) ->
          Grpc_eio_core.Body_parse.read_message ~pool:Connection.buffer_pool
            ~data ~len ~off msg_stream msg_state;
          Eio.Stream.add msg_stream None;
          Eio.Promise.resolve trailers_u trailers
    in

    let result_t, result_u = Eio.Promise.create () in
    let response_handler (response : Haha.Response.t) =
      let grpc_status_header =
        Haha.Headers.get_string (Haha.Response.headers response) "grpc-status"
      in
      let trailers =
        match grpc_status_header with
        | Some status ->
            Eio.Stream.add msg_stream None;
            Eio.Promise.create_resolved
            @@ Haha.Headers.of_list [ ("grpc-status", status) ]
        | None -> trailers_t
      in

      let rec get_next () =
        match Eio.Stream.take msg_stream with
        | Some msg -> Grpc_eio_core.Recv_seq.Next (msg, fun () -> get_next ())
        | None -> Done
      in

      let next : (response, 'err) Grpc_eio_core.Recv_seq.t =
        (fun () -> get_next ())
        |> Grpc_eio_core.Recv_seq.map
             (fun { Grpc_eio_core.Body_parse.consume } ->
               {
                 Grpc_eio_core.Body_reader.consume =
                   (fun f ->
                     consume (fun { Grpc_eio_core.Body_parse.bytes; len } ->
                         f (Pbrt.Decoder.of_subbytes bytes 0 len)));
               })
      in

      Eio.Promise.resolve result_u
        (Ok { Grpc_client_eio.Io.response; next; trailers });

      Haha.Response.handle ~on_data
    in

    let request =
      Haha.Request.create_with_streaming ~response_handler ~body_writer
        ~scheme:Connection.scheme ~authority:Connection.host
        ~headers:
          (Haha.Headers.of_list
             [ ("te", headers.te); ("content-type", headers.content_type) ])
        POST path
    in

    let fill_header_cs ~length (buffer : Cstruct.t) =
      Cstruct.set_char buffer 0 '\x00';
      Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
      Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)
    in

    let errored = Eio.Promise.is_resolved Connection.connection_error in
    if not errored then Connection.write_request request;
    let encoder = Pbrt.Encoder.create ~size:65536 () in
    ( (if errored then { Grpc_client_eio.Io.write = ignore; close = ignore }
       else
         {
           write =
             (let header_buffer = Cstruct.create 5 in
              (* TODO: should use some small buffer pool for body_buffer here *)
              let body_buffer = Cstruct.create 500_000 in
              fun (input : request) ->
                Pbrt.Encoder.clear encoder;
                input encoder;

                let length =
                  Pbrt.Encoder.blit_to_buffer
                    ~blit_from_bytes:Cstruct.blit_from_bytes encoder body_buffer
                    0
                in
                fill_header_cs ~length header_buffer;

                let css = [ header_buffer; Cstruct.sub body_buffer 0 length ] in

                write_body css);
           close = close_writer;
         }),
      result_t,
      Connection.connection_error )
end

let create ~sw ~net ?debug host : t * (unit -> unit) =
  let uri = Uri.of_string host in
  let scheme = Uri.scheme uri |> Option.value ~default:"http" in
  let host =
    match Uri.host uri with
    | None -> invalid_arg "No host in uri"
    | Some host -> host
  in
  let port =
    Uri.port uri
    |> Option.value
         ~default:
           (match scheme with
           | "http" -> 80
           | "https" -> 443
           | _ -> failwith "Don't know default port for this scheme")
  in

  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in

  let socket = Eio.Net.connect ~sw net addr in

  let req_stream = Eio.Stream.create 0 in
  let request_writer () = Eio.Stream.take req_stream in

  let write_end () = Eio.Stream.add req_stream None in

  let conn_err_t, conn_err_u = Eio.Promise.create () in

  Eio.Fiber.fork_daemon ~sw (fun () ->
      Haha.Client.run ?debug
        ~error_handler:(function
          | ConnectionError err ->
              Printf.printf "gRPC IO connection erra\n%!";
              Eio.Promise.resolve conn_err_u err
          | _ ->
              (* WARN: skill issue *)
              Printf.printf "gRPC IO stream erra\n%!")
        ~request_writer Haha.Settings.default socket;
      `Stop_daemon);

  let module Connection : Connection = struct
    let write_request req = Eio.Stream.add req_stream (Some req)
    let scheme = scheme
    let host = host
    let connection_error = conn_err_t
    let debug = Option.value ~default:false debug
    let buffer_pool = Grpc_eio_core.Buffer_pool.Bytes_pool.make ()
  end in
  ((module MakeHahaIO (Connection)), write_end)
