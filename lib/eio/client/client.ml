type 'a writer = { write : 'a -> unit; close : unit -> unit }

module Stream = Grpc_core_eio.Stream

type ('response, 'conn_error) connection = {
  writer : Net.writer;
  recv : ('response * Grpc_core_eio.Stream.t, 'conn_error) result Eio.Promise.t;
  grpc_status : Grpc.Status.t Eio.Promise.t;
}

type ('decoding_error, 'connection_error, 'response) error =
  [ `Decoding of 'decoding_error
  | `Rpc of 'response * Grpc.Status.t
  | `Connection of 'connection_error ]

type ('ok, 'decoding_error, 'connection_error, 'net_response) rpc_result =
  ('ok, ('decoding_error, 'connection_error, 'net_response) error) result

let call (type headers response conn_error) ~sw
    ~(net : (headers, response, conn_error) Net.t) ~service ~method_name
    ~(headers : Grpc_client.request_headers) () :
    ((response, conn_error) connection, conn_error) result =
  let (module Net) = net in
  let path = Grpc_client.make_path ~service ~method_name in
  match Net.send_request ~headers path with
  | Error conn_error -> Error conn_error
  | Ok (writer, recv_net) ->
      let status, status_notify = Eio.Promise.create () in
      let recv, recv_notify = Eio.Promise.create () in
      let () =
        Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve recv_notify
              (match Eio.Promise.await recv_net with
              | Error conn_error ->
                  Eio.Promise.resolve status_notify
                    (Grpc.Status.v ~message:"Connection error"
                       Grpc.Status.Unknown);
                  Error conn_error
              | Ok { response; body_reader; trailers } ->
                  Eio.Fiber.fork ~sw (fun () ->
                      Eio.Promise.resolve status_notify
                        (Grpc_client.status_of_trailers
                           ~get_header:
                             (Net.Headers.get (Eio.Promise.await trailers))));
                  Ok (response, body_reader)))
      in
      Ok { writer; recv; grpc_status = status }

let bidirectional_streaming (type headers response conn_error) ~sw
    ~(net : (headers, response, conn_error) Net.t) ~service ~method_name ~decode
    ~encode ~headers f :
    ('response, 'decoding_error, conn_error, response) rpc_result =
  match call ~sw ~net ~service ~method_name ~headers () with
  | Ok { writer; recv; grpc_status } -> (
      match Eio.Promise.await recv with
      | Ok (response, reader) ->
          let (module Net) = net in
          if Net.Response.is_ok response then (
            let decoding_error = ref None in
            let closed = ref false in
            let writer =
              {
                write = (fun msg -> writer.write (encode msg));
                close =
                  (fun () ->
                    writer.close ();
                    closed := true);
              }
            in

            let res =
              f ~writer ~take:(fun () ->
                  match Eio.Stream.take reader with
                  | None -> None
                  | Some t -> (
                      match decode t with
                      | Ok t -> Some t
                      | Error e ->
                          let () = decoding_error := Some e in
                          None))
            in
            if not !closed then writer.close ();
            match !decoding_error with
            | Some error -> Error (`Decoding error)
            | None -> (
                let status = Eio.Promise.await grpc_status in
                match Grpc.Status.code status with
                | Grpc.Status.OK -> Ok res
                | _ -> Error (`Rpc (response, status))))
          else Error (`Rpc (response, Eio.Promise.await grpc_status))
      | Error e -> Error (`Connection e))
  | Error e -> Error (`Connection e)

let server_streaming ~sw ~net ~service ~method_name ~decode ~encode ~headers
    request f =
  bidirectional_streaming ~sw ~net ~service ~method_name ~headers ~decode
    ~encode (fun ~writer ~take ->
      writer.write request;
      writer.close ();
      f ~read:(fun () -> take ()))

let client_streaming (type headers response conn_error) ~sw
    ~(net : (headers, response, conn_error) Net.t) ~service ~method_name ~decode
    ~encode ~headers f =
  match call ~sw ~net ~service ~method_name ~headers () with
  | Error e -> Error (`Connection e)
  | Ok { writer; recv; grpc_status } -> (
      f ~write:(fun msg -> writer.write (encode msg));
      writer.close ();
      match Eio.Promise.await recv with
      | Error e -> Error (`Connection e)
      | Ok (response, reader) -> (
          let resp = Eio.Stream.take reader |> Option.map decode in
          let grpc_status = Eio.Promise.await grpc_status in
          match (Grpc.Status.code grpc_status, resp) with
          | Grpc.Status.OK, Some (Ok decoded) -> Ok decoded
          | Grpc.Status.OK, Some (Error e) -> Error (`Decoding e)
          | Grpc.Status.OK, None ->
              Error
                (`Rpc
                   ( response,
                     Grpc.Status.v
                       ~message:"HTTP response is OK but body is empty"
                       Grpc.Status.Internal ))
          | _ -> Error (`Rpc (response, grpc_status))))

let unary ~sw ~net ~service ~method_name ~decode ~encode ~headers request =
  client_streaming ~sw ~net ~service ~method_name ~decode ~encode ~headers
    (fun ~write -> write request)
