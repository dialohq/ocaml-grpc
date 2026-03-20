(** Tests the invariant that {!Grpc_eio.Client.call} pipelines request DATA
    frames independently of response HEADERS.

    {b Normative basis}

    {ul
      {li {b \[GRPC-HTTP2\]} — gRPC over HTTP/2 protocol.
          {{:https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md}}

          The gRPC request/response flows are:

          {v
  Request  → Request-Headers *Length-Prefixed-Message EOS
  Response → Response-Headers *Length-Prefixed-Message Trailers
          v}

          No ordering constraint relates client DATA to server HEADERS; a
          conforming server may defer its HEADERS frame until after EOS.}

      {li {b \[RFC9113 §5.1\]} — HTTP/2 stream state machine.
          {{:https://www.rfc-editor.org/rfc/rfc9113#section-5.1}}

          A server in the "open" state (client HEADERS received, END_STREAM
          not yet received) is not required to send response HEADERS before
          the client sends END_STREAM.}

      {li {b \[GRPCIO-DISPATCH\]} — grpcio (Python) and grpc-core (C++)
          dispatch the service handler only after END_STREAM is received;
          those runtimes therefore cannot emit response HEADERS until the
          complete request stream has arrived.
          See also {{:https://github.com/grpc/grpc/issues/34893}}.}}

    {b Design note}

    Separating [write_body] (available immediately) from [read_body_p] (a
    promise for the response body) is the natural decomposition when the two
    directions are decoupled at the protocol level: the send path requires no
    knowledge of when HEADERS arrive.  {!Rpc.bidirectional_streaming} expresses
    this as two concurrent {!Eio.Fiber.both} fibers, the minimal structure
    needed to model the independence.

    {b Test structure}

    [deferred_response_handler] withholds response HEADERS until END_STREAM is
    received, instantiating the \[GRPCIO-DISPATCH\] constraint in-process.
    {!Grpc_eio.Client.call} is exercised via {!Grpc_eio.Client.Rpc.unary};
    the test asserts a clean round-trip with the expected payload and gRPC
    status. *)

(** [deferred_response_handler sw _addr reqd] reads the {b complete} request
    body before sending {b any} response HEADERS, realizing the
    \[GRPCIO-DISPATCH\] constraint: a server whose handler is dispatched only
    after END_STREAM cannot emit response HEADERS any earlier. *)
let deferred_response_handler sw _addr reqd =
  Eio.Fiber.fork ~sw (fun () ->
      let body = H2.Reqd.request_body reqd in
      let eof_p, eof_r = Eio.Promise.create () in
      (* h2 invokes these callbacks from its internal event loop; they must
         not block.  [Eio.Promise.resolve] is non-blocking and safe here. *)
      let rec on_read _bs ~off:_ ~len:_ =
        H2.Body.Reader.schedule_read body ~on_eof ~on_read
      and on_eof () = Eio.Promise.resolve eof_r () in
      H2.Body.Reader.schedule_read body ~on_read ~on_eof;
      (* Wait for END_STREAM before emitting any response HEADERS,
         per the [GRPCIO-DISPATCH] constraint. *)
      Eio.Promise.await eof_p;
      let resp_body =
        H2.Reqd.respond_with_streaming reqd ~flush_headers_immediately:true
          (H2.Response.create `OK
             ~headers:
               (H2.Headers.of_list
                  [ ("content-type", "application/grpc+proto") ]))
      in
      H2.Body.Writer.write_string resp_body (Grpc.Message.make "pong");
      H2.Reqd.schedule_trailers reqd
        (H2.Headers.of_list
           [
             ( "grpc-status",
               string_of_int (Grpc.Status.int_of_code Grpc.Status.OK) );
           ]);
      H2.Body.Writer.close resp_body)

let error_handler _addr ?request:_ _error _start_response = ()

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  (* Bind to an ephemeral loopback port so the test can run in parallel with
     other tests without port-allocation conflicts. *)
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr server_socket with
    | `Tcp (_, p) -> p
    | `Unix _ -> assert false
  in
  let result = ref None in
  Eio.Fiber.both
    (fun () ->
      (* Server: accept exactly one connection, handle it, then return.
         The connection closes once the client calls [H2_eio.Client.shutdown],
         at which point [create_connection_handler] returns and this fiber
         completes naturally — unblocking [Eio.Fiber.both]. *)
      let socket, addr = Eio.Net.accept ~sw server_socket in
      H2_eio.Server.create_connection_handler
        ~request_handler:(deferred_response_handler sw)
        ~error_handler addr ~sw socket)
    (fun () ->
      let socket =
        Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      let conn =
        H2_eio.Client.create_connection ~sw ~error_handler:ignore socket
      in
      (* The server above withholds response HEADERS until END_STREAM;
         [Grpc_eio.Client.call] must complete the round-trip regardless. *)
      result :=
        Some
          (Grpc_eio.Client.call ~service:"test.DeadlockCheck" ~rpc:"UnaryMethod"
             ~scheme:"http"
             ~do_request:(H2_eio.Client.request conn ~error_handler:ignore)
             ~handler:(Grpc_eio.Client.Rpc.unary "ping" ~f:(fun r -> r))
             ());
      Eio.Promise.await (H2_eio.Client.shutdown conn));
  match !result with
  | Some (Ok (Some response, status)) ->
      assert (response = "pong");
      assert (Grpc.Status.code status = Grpc.Status.OK);
      print_endline
        "PASS: unary call completed against deferred-response server"
  | Some (Ok (None, _)) -> failwith "FAIL: empty response body"
  | Some (Error s) ->
      failwith (Format.asprintf "FAIL: H2 error: %a" H2.Status.pp_hum s)
  | None -> failwith "FAIL: client fiber did not set result"
