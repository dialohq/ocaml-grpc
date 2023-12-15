open Grpc_eio

let sayHello rpc =
  Grpc_eio.Server.Typed_rpc.unary (Grpc_protoc.Server_rpc.unary rpc)
    ~f:(fun (request : Greeter_protoc.Greeter.hello_request) ->
      let message =
        if request.name = "" then "You forgot your name!"
        else Format.sprintf "Hello, %s!" request.name
      in
      let reply = Greeter_protoc.Greeter.default_hello_reply ~message () in
      (Grpc.Status.(v OK), Some reply))

let connection_handler server sw =
  let error_handler client_address ?request:_ _error start_response =
    Eio.traceln "Error in request from:%a" Eio.Net.Sockaddr.pp client_address;
    let response_body = start_response H2.Headers.empty in
    H2.Body.Writer.write_string response_body
      "There was an error handling your request.\n";
    H2.Body.Writer.close response_body
  in
  let request_handler client_address request_descriptor =
    Eio.traceln "Handling a request from:%a" Eio.Net.Sockaddr.pp client_address;
    Eio.Fiber.fork ~sw (fun () ->
        Grpc_eio.Server.handle_request server request_descriptor)
  in
  fun socket addr ->
    H2_eio.Server.create_connection_handler ?config:None ~request_handler
      ~error_handler addr ~sw socket

let serve server env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let handler = connection_handler server sw in
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let rec listen () =
    Eio.Net.accept_fork ~sw server_socket
      ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
      handler;
    listen ()
  in
  Printf.printf "Listening on port %i for grpc requests\n" port;
  print_endline "";
  print_endline "Try running:";
  print_endline "";
  print_endline
    {| dune exec -- examples/greeter-protoc-client-eio/greeter_client_eio.exe <your_name> |};
  listen ()

let () =
  let server =
    Greeter_protoc.Greeter.Greeter.Server.make ~sayHello ()
    |> Grpc_protoc.handlers |> Server.Typed_rpc.server
  in

  Eio_main.run (serve server)
