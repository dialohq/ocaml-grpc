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
    let addr = `Tcp (Eio_unix.Ipaddr.of_unix inet, port) in
    let socket = Eio.Net.connect ~sw network addr in
    let connection =
      H2_eio.Client.create_connection ~sw ~error_handler:ignore
        (socket :> Eio.Flow.two_way)
    in

    let encoder = Pbrt.Encoder.create () in
    let request = Greeter.Greeter_types.default_hello_request ~name () in
    Greeter.Greeter_pb.encode_hello_request request encoder;
    let encoded_request = Pbrt.Encoder.to_string encoder in
    let f = function
      | Some response ->
          let decoded_response = Pbrt.Decoder.of_string response in
          Greeter.Greeter_pb.decode_hello_reply decoded_response
      | None -> Greeter.Greeter_types.default_hello_reply ()
    in

    let result =
      Grpc_eio.Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
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
  | Ok ({ message }, status) ->
      Eio.traceln "%s: %s" (Grpc.Status.show status) message
  | Error err -> Eio.traceln "Error: %a" H2.Status.pp_hum err
