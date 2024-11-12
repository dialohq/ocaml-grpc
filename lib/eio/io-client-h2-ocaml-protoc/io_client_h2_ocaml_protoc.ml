module type Client = sig
  val connection : H2_eio.Client.t

  (* This promise might eventually resolve at any point so we should handle it everywhere *)
  val connection_error : H2.Client_connection.error Eio.Promise.t
  val host : string
  val scheme : string
end

module Headers = struct
  type t = H2.Headers.t

  let get = H2.Headers.get
end

module Net_response = struct
  type t = H2.Response.t

  let is_ok t = H2.Status.is_successful t.H2.Response.status
  let headers t = t.H2.Response.headers
end

type connection_error = H2.Client_connection.error

type stream_error =
  [ `Unexpected_eof | `Connection_error of H2.Client_connection.error ]

type t =
  ( H2.Headers.t,
    H2.Response.t,
    Pbrt.Encoder.t -> unit,
    Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
    stream_error,
    H2.Client_connection.error )
  Grpc_client_eio.Io.t

type exn += Write_after_error

module Growing_buffer = Grpc.Buffer

(* type resp_consumer = { consume : 'a. (Pbrt.Decoder.t -> 'a) -> 'a } *)

module Make_net (Client : Client) :
  Grpc_client_eio.Io.S
    with type Net_response.t = H2.Response.t
     and type Headers.t = H2.Headers.t
     and type connection_error = connection_error
     and type request = Pbrt.Encoder.t -> unit
     and type response = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type stream_error = stream_error = struct
  module Net_response = Net_response
  module Headers = Headers

  type nonrec connection_error = connection_error
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

  let send_request ~(headers : Grpc_client.request_headers) target =
    (* We are flushing headers immediately but potentially for the
     unary and server streaming cases we shouldn't do it
  *)
    let request =
      H2.Request.create ~scheme:Client.scheme `POST target
        ~headers:
          (H2.Headers.of_list
             [
               (":authority", Client.host);
               ("te", headers.te);
               ("content-type", headers.content_type);
             ])
    in
    (* Refs are used in order to prevent from leaky promises. 
       I find promises that never get resolved a bit of an anti-pattern
    *)
    let result, result_u = Eio.Promise.create () in
    let trailers_handler = ref ignore in
    let error_handler =
      ref (fun error -> Eio.Promise.resolve_error result_u error)
    in
    (* Allocate once, use a pool of these *)
    let errored = ref false in
    let response_handler response reader =
      let trailers, trailers_u = Eio.Promise.create () in
      let () =
        trailers_handler :=
          fun trailers -> Eio.Promise.resolve trailers_u trailers
      in

      let next =
        (fun () ->
          Grpc_eio_core.Body_reader.read_next ~error:Client.connection_error
            (H2.Body.Reader.schedule_read reader))
        |> Grpc_eio_core.Recv_seq.map
             (fun { Grpc_eio_core.Body_reader.consume } ->
               {
                 Grpc_eio_core.Body_reader.consume =
                   (fun f ->
                     consume (fun { Grpc_eio_core.Body_reader.bytes; len } ->
                         f (Pbrt.Decoder.of_subbytes bytes 0 len)));
               })
      in

      Eio.Promise.resolve result_u
        (Ok { Grpc_client_eio.Io.response; next; trailers })
    in
    let body_writer =
      H2_eio.Client.request ~flush_headers_immediately:true Client.connection
        ~trailers_handler:(fun trailers -> !trailers_handler trailers)
        ~error_handler:(fun error -> !error_handler error)
        ~response_handler request
    in
    let encoder = Pbrt.Encoder.create ~size:65536 () in
    ( {
        Grpc_client_eio.Io.write =
          (let header_buffer = Bytes.create 5 in

           fun input ->
             if !errored = true then raise Write_after_error
             else (
               Pbrt.Encoder.clear encoder;
               input encoder;

               let msg = Pbrt.Encoder.to_bytes encoder in

               Grpc.Message.fill_header ~length:(Bytes.length msg) header_buffer;

               H2.Body.Writer.write_string body_writer
                 (Bytes.unsafe_to_string header_buffer);

               H2.Body.Writer.write_string body_writer
                 (Bytes.unsafe_to_string msg)));
        close = (fun () -> H2.Body.Writer.close body_writer);
      },
      result,
      Client.connection_error )
end

module Expert = struct
  let create_with_socket ~sw ~(socket : _ Eio.Net.stream_socket) ~host ~scheme :
      t =
    let connection, connection_resolve = Eio.Promise.create () in
    let connection_error, connection_error_resolve = Eio.Promise.create () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Switch.run (fun sw' ->
            let conn =
              H2_eio.Client.create_connection ~sw:sw'
                ~error_handler:(fun e ->
                  Eio.Promise.resolve connection_error_resolve e)
                socket
            in

            Eio.Switch.on_release sw' (fun () ->
                Eio.Promise.await (H2_eio.Client.shutdown conn));
            (* For now we're ignoring the errors, we should probably inject them into grpc handlers to let them handle it *)
            Eio.Promise.resolve connection_resolve conn);
        `Stop_daemon);
    let conn = Eio.Promise.await connection in
    (module Make_net (struct
      let connection = conn
      let connection_error = connection_error
      let host = host
      let scheme = scheme
    end))

  let create_with_address ~(net : Eio_unix.Net.t) ~sw ~scheme ~host ~port =
    let inet, port =
      Eio_unix.run_in_systhread (fun () ->
          Unix.getaddrinfo host (string_of_int port)
            [ Unix.(AI_FAMILY PF_INET) ])
      |> List.filter_map (fun (addr : Unix.addr_info) ->
             match addr.ai_addr with
             | Unix.ADDR_UNIX _ -> None
             | ADDR_INET (addr, port) -> Some (addr, port))
      |> List.hd
    in
    let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in

    let socket = Eio.Net.connect ~sw net addr in

    create_with_socket ~socket ~host ~scheme ~sw
end

let create_client ~net ~sw addr =
  let uri = Uri.of_string addr in
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
  Expert.create_with_address ~net ~sw ~scheme ~host ~port
