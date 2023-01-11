open Grpc_eio

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
  (Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder))

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
      ~error_handler addr socket

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
    {| dune exec -- examples/greeter-client-lwt/greeter_client_lwt.exe <your_name> |};
  listen ()

let () =
  let greeter_service =
    Server.Service.(
      v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)
  in
  let server =
    Server.(
      v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)
  in
  Eio_main.run (serve server)