open! Core
open! Async

let call_server address port req =
  let%bind socket =
    let%bind addresses =
      Unix.Addr_info.get ~host:address ~service:(Int.to_string port)
        [ Unix.Addr_info.AI_FAMILY Unix.PF_INET ]
    in
    let socket = Unix.Socket.create Unix.Socket.Type.tcp in
    let address =
      let sockaddr =
        match addresses with
        | hd :: _ -> hd.Unix.Addr_info.ai_addr
        | [] -> failwithf "call_server: no address for %s %d" address port ()
      in
      match sockaddr with
      | Unix.ADDR_INET (a, i) -> `Inet (a, i)
      | ADDR_UNIX u ->
          failwithf "can't make an Socket.Address.Inet out of a UNIX socket %s"
            u ()
    in
    Unix.Socket.connect socket address
  in
  let error_handler = function
    | `Invalid_response_body_length _resp ->
        printf "invalid response body length\n%!"
    | `Exn _exn -> printf "exception!\n%!"
    | `Malformed_response s -> printf "malformed response: %s\n%!" s
    | `Protocol_error (code, s) ->
        printf "protocol error: %s, %s\n" (H2.Error_code.to_string code) s
  in
  let%bind connection =
    H2_async.Client.create_connection ~error_handler socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_request req enc;
  Grpc_async.Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
    ~handler:
      (Grpc_async.Client.Rpc.unary (Pbrt.Encoder.to_string enc)
         ~f:(fun decoder ->
           match%map decoder with
           | `Ok decoder ->
               let decoder = Pbrt.Decoder.of_string decoder in
               Greeter.Greeter_pb.decode_hello_reply decoder
           | `Eof -> Greeter.Greeter_types.default_hello_reply ()))
    ()

let () =
  let port = 8080 in
  let address = "localhost" in
  let name =
    match Sys.get_argv () with [| _arg0; name |] -> name | _ -> "anonymous"
  in
  let req = Greeter.Greeter_types.default_hello_request ~name () in
  don't_wait_for
    (let%bind res = call_server address port req in
     match res with
     | Ok (res, _) ->
         printf "%s\n%!" res.message;
         return ()
     | Error _ ->
         printf "an error occurred\n";
         return ());
  never_returns (Scheduler.go ())
