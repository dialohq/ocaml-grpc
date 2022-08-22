open! Core
open! Async

let error_handler = function
  | `Invalid_response_body_length _resp ->
      printf "invalid response body length\n%!"
  | `Exn _exn -> printf "exception!\n%!"
  | `Malformed_response s -> printf "malformed response: %s\n%!" s
  | `Protocol_error (code, s) ->
      printf "protocol error: %s, %s\n" (H2.Error_code.to_string code) s

let call_server ~host ~port req =
  let socket = Unix.Socket.create Unix.Socket.Type.tcp in
  let where_to_connect =
    let hnp = Host_and_port.create ~host ~port in
    Tcp.Where_to_connect.of_host_and_port hnp
  in
  H2_async.Client.TLS.create_connection_with_default ~error_handler socket
    where_to_connect
  >>= fun tls_conn ->
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_request req enc;
  Grpc_async.Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_async.Client.TLS.request tls_conn ~error_handler:ignore)
    ~handler:
      (Grpc_async.Client.Rpc.unary ~encoded_request:(Pbrt.Encoder.to_string enc)
         ~handler:(function
        | None -> return (Greeter.Greeter_types.default_hello_reply ())
        | Some response ->
            let response =
              Pbrt.Decoder.of_string response
              |> Greeter.Greeter_pb.decode_hello_reply
            in
            return response))
    ()

let main port host () =
  let name = "hello" in
  let req = Greeter.Greeter_types.default_hello_request ~name () in
  don't_wait_for
    (let%bind res = call_server ~host ~port req in
     match res with
     | Ok (res, _) ->
         printf "%s\n%!" res.message;
         return ()
     | Error _ ->
         printf "an error occurred\n";
         return ());
  Deferred.never ()

let () =
  let cmd =
    Command.async_spec ~summary:"Start a greeter tls-async client"
      Command.Spec.(
        empty
        +> flag "-port"
             (optional_with_default 8080 int)
             ~doc:"int Source port to listen on"
        +> flag "-host"
             (optional_with_default "localhost" string)
             ~doc:"HOST to connect to")
      main
  in
  Command_unix.run cmd
