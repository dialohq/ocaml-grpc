open Grpc_lwt

let say_hello buffer =
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let decode, encode = Service.make_service_functions Greeter.sayHello in
  let request =
    Reader.create buffer |> decode |> function
    | Ok v -> v
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  let message =
    if request = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" request
  in
  let reply = Greeter.SayHello.Response.make ~message () in
  Lwt.return (Grpc.Status.(v OK), Some (encode reply |> Writer.contents))

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
      let error_handler :
          Unix.sockaddr ->
          ?request:H2.Request.t ->
          _ ->
          (H2.Headers.t -> H2.Body.Writer.t) ->
          unit =
       fun _client_address ?request:_ _error start_response ->
        let response_body = start_response H2.Headers.empty in
        H2.Body.Writer.close response_body
      in
      let server =
        let certfile = "./certificates/server.pem" in
        let keyfile = "./certificates/server.key" in
        H2_lwt_unix.Server.SSL.create_connection_handler_with_default ~certfile
          ~keyfile ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| dune exec -- examples/greeter-server-ssl-lwt/greeter_client_ssl_lwt.exe <your_name> |});

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
