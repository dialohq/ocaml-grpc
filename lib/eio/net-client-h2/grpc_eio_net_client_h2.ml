module type Client = sig
  val connection : H2_eio.Client.t Eio.Promise.t
  val host : string
  val scheme : string
end

exception Network_error_todo_remove of H2.Client_connection.error

module Response = struct
  type t = H2.Response.t

  let is_ok response = response.H2.Response.status = `OK
  let headers response = response.H2.Response.headers
end

module Headers = struct
  type t = H2.Headers.t

  let get headers key = H2.Headers.get headers key
end

type connection_error = H2.Client_connection.error

module Make_net (Client : Client) :
  Grpc_client_eio.Net.S
    with type Response.t = H2.Response.t
     and type Headers.t = H2.Headers.t
     and type connection_error = connection_error = struct
  module Response = Response
  module Headers = Headers

  type nonrec connection_error = connection_error

  let send_request ~(headers : Grpc_client.request_headers) target =
    (* We are flushing headers immediately but potentially for the
     unary and server streaming cases we shouldn't do it
  *)
    let reader_a, reader_u = Eio.Promise.create () in
    let trailers_a, trailers_u = Eio.Promise.create () in
    let trailers_handler trailers = Eio.Promise.resolve trailers_u trailers in
    let response_handler response body =
      let body_reader =
        Grpc_core_eio.Stream.make
          ~schedule_read_raw:(fun ~on_eof:on_eof_stream ~on_read ->
            let on_eof () =
              if not (Eio.Promise.is_resolved trailers_a) then
                Eio.Promise.resolve trailers_u response.H2.Response.headers;
              on_eof_stream ()
            in
            H2.Body.Reader.schedule_read body ~on_eof ~on_read)
      in
      Eio.Promise.resolve_ok reader_u
        { Grpc_client_eio.Net.response; body_reader; trailers = trailers_a }
    in
    let error_handler error =
      if Eio.Promise.is_resolved reader_a then
        raise (Network_error_todo_remove error)
      else Eio.Promise.resolve_error reader_u error
    in
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
    let body_writer =
      H2_eio.Client.request ~flush_headers_immediately:true
        (Eio.Promise.await Client.connection)
        ~trailers_handler ~response_handler ~error_handler request
    in
    let writer =
      {
        Grpc_client_eio.Net.write =
          (fun input ->
            H2.Body.Writer.write_string body_writer (Grpc.Message.make input));
        close = (fun () -> H2.Body.Writer.close body_writer);
      }
    in
    Ok (writer, reader_a)
end

module Expert = struct
  let create_with_socket ~sw ~(socket : _ Eio.Net.stream_socket) ~host ~scheme :
      (Headers.t, Response.t, connection_error) Grpc_client_eio.Net.t =
    let connection, connection_resolve = Eio.Promise.create () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Switch.run (fun sw' ->
            let conn =
              H2_eio.Client.create_connection ~sw:sw' ~error_handler:ignore
                socket
            in
            Eio.Switch.on_release sw' (fun () ->
                Eio.Promise.await (H2_eio.Client.shutdown conn));
            (* For now we're ignoring the errors, we should probably inject them into grpc handlers to let them handle it *)
            Eio.Promise.resolve connection_resolve conn);
        `Stop_daemon);
    (module Make_net (struct
      let connection = connection
      let host = host
      let scheme = scheme
    end))

  (* TODO: Connection management *)
  let create_with_address ~(net : Eio_unix.Net.t) ~sw ~scheme ~host ~port :
      (Headers.t, Response.t, connection_error) Grpc_client_eio.Net.t =
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
