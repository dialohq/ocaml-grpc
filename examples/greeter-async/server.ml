open Async
open Grpc_async

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
    if String.equal request "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" request
  in
  let reply = Greeter.SayHello.Response.make ~message () in
  Async.return (Grpc.Status.(v OK), Some (encode reply |> Writer.contents))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)

let main port () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  Tcp.(Server.create_sock ~on_handler_error:`Ignore where_to_listen)
    (H2_async.Server.create_connection_handler
       ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
       ~error_handler:(fun _ ?request:_ _ _ ->
         print_endline "an error occurred"))
  >>= fun _server -> Deferred.never ()

let () =
  Command.async_spec ~summary:"Start a hello world async_ssl server"
    Command.Spec.(
      empty
      +> flag "-port"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on")
    main
  |> Command_unix.run
