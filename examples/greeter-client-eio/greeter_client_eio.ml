let main env =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let host = "localhost" in
  let port = "8080" in
  let network = Eio.Stdenv.net env in
  let run sw =
    let inet, port =
      Eio_unix.run_in_systhread (fun () ->
          Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
      |> List.filter_map (fun (addr : Unix.addr_info) ->
             match addr.ai_addr with
             | Unix.ADDR_UNIX _ -> None
             | ADDR_INET (addr, port) -> Some (addr, port))
      |> List.hd
    in
    let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
    let socket = Eio.Net.connect ~sw network addr in
    let connection =
      H2_eio.Client.create_connection ~sw ~error_handler:ignore socket
    in

    let open Ocaml_protoc_plugin in
    let open Greeter.Mypackage in
    let encode, decode = Service.make_client_functions Greeter.sayHello in
    let encoded_request =
      HelloRequest.make ~name () |> encode |> Writer.contents
    in

    let f decoder =
      match decoder with
      | Some decoder -> (
          Reader.create decoder |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e)))
      | None -> Greeter.SayHello.Response.make ()
    in

    let result =
      Grpc_eio.Client.Rpc.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
        ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
        ~handler:(Grpc_eio.Client.Rpc.unary encoded_request ~f)
        ()
    in
    Eio.Promise.await (H2_eio.Client.shutdown connection);
    result
  in
  Eio.Switch.run run

let () =
  match Eio_main.run main with
  | Ok (message, status) ->
      Eio.traceln "%s: %s" (Grpc.Status.show status) message
  | Error err -> Eio.traceln "Error: %a" H2.Status.pp_hum err
