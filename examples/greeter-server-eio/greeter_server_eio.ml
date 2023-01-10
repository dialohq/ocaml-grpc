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

let request_loop server queue =
  Eio.Switch.run @@ fun sw ->
  let rec aux () =
    let request = Eio.Stream.take queue in
    Eio.Fiber.fork ~sw (fun () -> Grpc_eio.Server.handle_request server request);
    aux ()
  in
  aux ()

let connection_handler queue =
  let error_handler _ ?request:_ _ _ = assert false in
  let request_handler client_address request_descriptor =
    Eio.traceln "Handling a request from:%a" Eio.Net.Sockaddr.pp client_address;
    Eio.Stream.add queue request_descriptor
  in
  fun socket addr ->
    H2_eio.Server.create_connection_handler ?config:None ~request_handler
      ~error_handler addr socket

let serve server env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let queue = Eio.Stream.create 10 in
  Eio.Fiber.both
    (fun () -> request_loop server queue)
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.fork ~sw @@ fun () ->
      let server_socket =
        Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
      in
      let rec listen () =
        Eio.Net.accept_fork ~sw server_socket
          ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
          (connection_handler queue);
        listen ()
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| dune exec -- examples/greeter-client-lwt/greeter_client_lwt.exe <your_name> |};
      listen ())

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