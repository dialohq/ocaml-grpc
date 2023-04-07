open! Core
open! Async

let error_handler = function
  | `Invalid_response_body_length _resp ->
      printf "invalid response body length\n%!"
  | `Exn _exn -> printf "exception!\n%!"
  | `Malformed_response s -> printf "malformed response: %s\n%!" s
  | `Protocol_error (code, s) ->
      printf "protocol error: %s, %s\n" (H2.Error_code.to_string code) s

let create_connection address port =
  (* Create separately for reuse. *)
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
  H2_async.Client.create_connection ~error_handler socket

let call_server connection req =
  let enc = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_request req enc;
  let handler = function
    | None -> return (Greeter.Greeter_types.default_hello_reply ())
    | Some response ->
        let response =
          Pbrt.Decoder.of_string response
          |> Greeter.Greeter_pb.decode_hello_reply
        in
        return response
  in
  Grpc_async.Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
    ~handler:
      (Grpc_async.Client.Rpc.unary
         ~encoded_request:(Pbrt.Encoder.to_string enc)
         ~handler)
    ()

let () =
  let port = 8080 in
  let address = "localhost" in
  let name =
    match Sys.get_argv () with [| _arg0; name |] -> name | _ -> "anonymous"
  in
  let next_name counter = sprintf "%s-%i" name counter in
  let check = function
    | Ok (res, status) -> (
        match Grpc.Status.code status with
        | Grpc.Status.Unknown ->
            (* If this happens, it's a bug. And it does happen. *)
            failwithf "Got unexpected status = Unknown for res = %s: %s"
              res.Greeter.Greeter_types.message (Grpc.Status.show status) ()
        | _ ->
            (* printf "%s\n%!" res.Greeter.Greeter_types.message; *)
            ())
    | Error _ ->
        printf "an error occurred\n";
        ()
  in
  let rec do_req connection counter =
    (* Should blow up before that count with very high probability, and this helps
       when testing the solution. *)
    if Int.(counter > 10_000) then return ()
    else
      let name = next_name counter in
      let req = Greeter.Greeter_types.default_hello_request ~name () in
      let%bind res = call_server connection req in
      check res;
      do_req connection (counter + 1)
  in
  Deferred.don't_wait_for
    (let%bind connection = create_connection address port in
     do_req connection 0);
  never_returns (Scheduler.go ())
