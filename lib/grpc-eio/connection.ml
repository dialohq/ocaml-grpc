let grpc_recv_streaming body = Stream.(of_h2_body body)

let grpc_send_streaming_client body =
  let send input =
    let payload = Grpc.Message.make input in
    H2.Body.Writer.write_string body payload
  in
  let close () = H2.Body.Writer.close body in
  (send, close)

let grpc_send_streaming request =
  let body =
    H2.Reqd.respond_with_streaming ~flush_headers_immediately:true request
      (H2.Response.create
         ~headers:
           (H2.Headers.of_list [ ("content-type", "application/grpc+proto") ])
         `OK)
  in
  let on_msg input =
    let payload = Grpc.Message.make input in
    H2.Body.Writer.write_string body payload
  in
  let on_eof status =
    (try
       H2.Reqd.schedule_trailers request
         (H2.Headers.of_list
            ([
               ( "grpc-status",
                 string_of_int
                   (Grpc.Status.int_of_code (Grpc.Status.code status)) );
             ]
            @
            match Grpc.Status.message status with
            | None -> []
            | Some message -> [ ("grpc-message", message) ]))
     with
     | ((Failure "h2.Reqd.schedule_trailers: stream already closed")
     [@warning "-52"] (* https://github.com/anmonteiro/ocaml-h2/issues/175 *))
     ->
       ());
    H2.Body.Writer.close body
  in
  (on_msg, on_eof)
