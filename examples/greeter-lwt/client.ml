(* $MDX part-begin=client-imports *)
open Grpc_lwt
open Lwt.Syntax
(* $MDX part-end *)

(* $MDX part-begin=client-hello *)
let call_server address port req =
  (* Setup Http/2 connection *)
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
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let encode, decode = Service.make_client_functions Greeter.sayHello in
  let enc = encode req |> Writer.contents in

  Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder -> (
               Reader.create decoder |> decode |> function
               | Ok v -> v
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> Greeter.SayHello.Response.make ()))
    ()
(* $MDX part-end *)

(* $MDX part-begin=client-main *)
let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Greeter.Mypackage.HelloRequest.make ~name () in
  Lwt_main.run
    (let+ res = call_server address port req in
     match res with
     | Ok (res, _) -> print_endline res
     | Error _ -> print_endline "an error occurred")
(* $MDX part-end *)