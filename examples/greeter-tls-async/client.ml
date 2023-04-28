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
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let decode, encode = Service.make_service_functions Greeter.sayHello in
  let enc = encode req |> Writer.contents in
  Grpc_async.Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_async.Client.TLS.request tls_conn ~error_handler:ignore)
    ~handler:
      (Grpc_async.Client.Rpc.unary ~encoded_request:enc ~handler:(function
        | None -> return (Greeter.SayHello.Response.make ())
        | Some response ->
            let response =
              Reader.create response |> decode |> function
              | Ok v -> v
              | Error e ->
                  failwith
                    (Printf.sprintf "Could not decode request: %s"
                       (Result.show_error e))
            in
            return response))
    ()

let main port host () =
  let name = "hello" in
  let req = Greeter.Mypackage.HelloRequest.make ~name () in
  don't_wait_for
    (let%bind res = call_server ~host ~port req in
     match res with
     | Ok (res, _) ->
         printf "%s\n%!" res;
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
