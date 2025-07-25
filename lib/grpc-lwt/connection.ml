open Lwt.Syntax

let grpc_recv_streaming body buffer_push =
  let request_buffer = Grpc.Buffer.v () in
  let on_eof () = buffer_push None in
  let rec on_read buffer ~off ~len =
    Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src:buffer
      ~dst:request_buffer ~length:len;
    Grpc.Message.extract_all (fun msg -> buffer_push (Some msg)) request_buffer;
    H2.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  H2.Body.Reader.schedule_read body ~on_read ~on_eof

let grpc_send_streaming_client body encoder_stream =
  let+ () =
    Lwt_stream.iter
      (fun encoder ->
        let payload = Grpc.Message.make encoder in
        H2.Body.Writer.write_string body payload)
      encoder_stream
  in
  H2.Body.Writer.close body

let grpc_send_streaming request encoder_stream status_mvar =
  let body =
    H2.Reqd.respond_with_streaming ~flush_headers_immediately:true request
      (H2.Response.create
         ~headers:
           (H2.Headers.of_list [ ("content-type", "application/grpc+proto") ])
         `OK)
  in
  let* () =
    (* bind *)
    Lwt_stream.iter
      (fun input ->
        let payload = Grpc.Message.make input in
        H2.Body.Writer.write_string body payload;
        H2.Body.Writer.flush body (fun _ -> ()))
      encoder_stream
  in
  let+ status = Lwt_mvar.take status_mvar in
  (* map *)
  H2.Reqd.schedule_trailers request
    (H2.Headers.of_list
       ([
          ( "grpc-status",
            string_of_int (Grpc.Status.int_of_code (Grpc.Status.code status)) );
        ]
       @
       match Grpc.Status.message status with
       | None -> []
       | Some message -> [ ("grpc-message", message) ]));
  H2.Body.Writer.close body
