open Grpc_lwt
open Lwt.Syntax

let call_server address port req =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    H2_lwt_unix.Client.create_connection ~error_handler socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_request req enc;

  Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
           Lwt.return
           @@
           match decoder with
           | Some decoder -> Greeter.Greeter_pb.decode_hello_reply decoder
           | None -> Greeter.Greeter_types.default_hello_reply ()))
    ()

let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Greeter.Greeter_types.default_hello_request ~name () in
  Lwt_main.run
    (let+ res = call_server address port req in
     print_endline res.message)
