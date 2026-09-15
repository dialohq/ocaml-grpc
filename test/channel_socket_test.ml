open Eio.Std

let fd_count () =
  let path =
    if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"
  in
  Array.length (Sys.readdir path)

let await_fd_count clock expected =
  let rec loop remaining =
    let actual = fd_count () in
    if actual > expected then
      if remaining = 0 then
        failwith
          (Printf.sprintf "Expected at most %d open fds, found %d" expected
             actual)
      else (
        Eio.Time.sleep clock 0.01;
        loop (remaining - 1))
  in
  loop 100

let run_case env name handler expect_success =
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let listener =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:16 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let stop, stop_resolver = Promise.create () in
  Fiber.fork ~sw (fun () ->
      Eio.Net.run_server ~stop ~on_error:raise listener handler);
  Fiber.yield ();
  ignore
    (Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net "127.0.0.1");
  Eio.Time.sleep clock 0.01;
  let baseline = fd_count () in
  for _ = 1 to 100 do
    let channel =
      Grpc.Channel.create ~sw ~net (Printf.sprintf "http://127.0.0.1:%d" port)
    in
    let result =
      Grpc.Client.Unary.call ~channel ~service:"test" ~method_name:"echo"
        (fun _ -> ())
    in
    Grpc.Channel.shutdown channel;
    (match (result, expect_success) with
    | Ok _, true | Error _, false -> ()
    | Ok _, false -> failwith "Expected RPC failure"
    | Error _, true -> failwith "Expected successful RPC");
    await_fd_count clock baseline
  done;
  Promise.resolve stop_resolver ();
  Printf.printf "%s: 100 requests without fd growth\n%!" name

let completed_request =
  Grpc.Server.connection_handler (fun ~service:_ ~meth:_ ->
      Some (Grpc.Server.Unary.respond (fun _ _ -> ())))

let preface_failure socket _ =
  let preface = Cstruct.create 24 in
  Eio.Flow.read_exact socket preface

let connection_failure socket _ =
  let preface = Cstruct.create 24 in
  Eio.Flow.read_exact socket preface;
  Eio.Flow.copy_string "\000\000\000\004\000\000\000\000\000" socket;
  let rec await_request () =
    let header = Cstruct.create 9 in
    Eio.Flow.read_exact socket header;
    let length =
      (Cstruct.get_uint8 header 0 lsl 16)
      lor (Cstruct.get_uint8 header 1 lsl 8)
      lor Cstruct.get_uint8 header 2
    in
    Eio.Flow.read_exact socket (Cstruct.create length);
    if Cstruct.get_uint8 header 3 <> 1 then await_request ()
  in
  await_request ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15.0 (fun () ->
      run_case env "completed request" completed_request true;
      run_case env "preface failure" preface_failure false;
      run_case env "connection failure" connection_failure false)
