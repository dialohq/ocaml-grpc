module Server = Grpc_server_eio
module Net = Grpc_eio_net_server_h2

let say_hello env buffer =
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
  Eio.Time.sleep env#clock 10.0;
  (Grpc_server.trailers_with_code OK, Some (encode reply |> Writer.contents))

let serve server env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let connection_handler client_addr socket =
    Eio.Switch.run (fun sw ->
        Net.connection_handler ~sw server client_addr socket)
  in
  Eio.Net.run_server
    ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
    server_socket connection_handler

let mk_handler f =
  { Grpc_server_eio.Rpc.headers = (fun _ -> Grpc_server.headers `Proto); f }

let server env =
  let add_rpc = Server.Service.add_rpc in
  let open Server.Rpc in
  let service =
    Server.Service.v ()
    |> add_rpc ~name:"SayHello" ~rpc:(mk_handler (unary (say_hello env)))
  in
  Server.(make () |> add_service ~name:"mypackage.Greeter" ~service)

let () = Eio_main.run (fun env -> serve (server env) env)
