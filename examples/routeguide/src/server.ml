open Grpc_lwt

let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let* _server = Lwt_io.establish_server_with_client_socket listen_address server in
      let* () = Lwt_io.printf "Listening on port %i for grpc requests\n" port in
      Lwt_io.(flush stdout));

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
