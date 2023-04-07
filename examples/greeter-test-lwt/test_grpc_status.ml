open Grpc_lwt
open Lwt.Syntax

let create_connection address port =
  let error_handler _ = print_endline "error" in
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let call_server connection req =
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter.Greeter_pb.encode_hello_request req enc;
  Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary (Pbrt.Encoder.to_string enc) ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder ->
               let decoder = Pbrt.Decoder.of_string decoder in
               Greeter.Greeter_pb.decode_hello_reply decoder
           | None -> Greeter.Greeter_types.default_hello_reply ()))
    ()

let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let next_name counter = Printf.sprintf "%s-%i" name counter in
  let check = function
    | Ok (res, status) -> (
        match Grpc.Status.code status with
        | Grpc.Status.Unknown ->
            (* If this happens, it's a bug. And it does happen. *)
            let msg =
              Printf.sprintf "Got unexpected status = Unknown for res = %s: %s"
                res.Greeter.Greeter_types.message (Grpc.Status.show status)
            in
            raise (Failure msg)
        | _ ->
            (* printf "%s\n%!" res.Greeter.Greeter_types.message; *)
            ())
    | Error _ ->
        Printf.printf "an error occurred\n";
        ()
  in
  let rec do_req connection counter =
    let name = next_name counter in
    let req = Greeter.Greeter_types.default_hello_request ~name () in
    let* res = call_server connection req in
    check res;
    do_req connection (counter + 1)
  in
  Lwt_main.run
    (let* connection = create_connection address port in
     do_req connection 0)
