open Grpc_lwt

let say_hello buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  let req = Greeter.Greeter_pb.decode_hello_request decoder in
  let message =
    if req.name = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" req.name
  in
  let reply = Greeter.Greeter_types.default_hello_reply ~message () in
  let encoder = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_reply reply encoder;
  Lwt.return (Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)

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
