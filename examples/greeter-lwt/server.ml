(* $MDX part-begin=server-imports *)
open Grpc_lwt
(* $MDX part-end *)

(* $MDX part-begin=server-hello *)
let say_hello buffer =
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let (decode, encode) = Service.make_service_functions Greeter.sayHello in
  let request =
    Reader.create buffer
    |> decode
    |> function
      | Ok v -> v
      | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  let message =
    if request = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" request
  in
  let reply = Greeter.SayHello.Response.make ~message () in
  Lwt.return (Grpc.Status.(v OK), Some (encode reply |> Writer.contents ))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)
(* $MDX part-end *)

(* $MDX part-begin=server-main *)
let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| dune exec -- examples/greeter-client-lwt/greeter_client_lwt.exe <your_name> |});

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
(* $MDX part-end *)
