open! Core
open! Async

let grpc_recv_streaming body buffer_push =
  let request_buffer = Grpc.Buffer.v () in
  let on_eof () = Async.Pipe.close buffer_push in
  let rec on_read buffer ~off ~len =
    Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src:buffer
      ~dst:request_buffer ~length:len;
    let message = Grpc.Message.extract request_buffer in
    (match message with
    | Some message -> Async.Pipe.write_without_pushback buffer_push message
    | None -> ());
    H2.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  H2.Body.Reader.schedule_read body ~on_read ~on_eof

let grpc_send_streaming_client body encoder_stream =
  let%map () =
    Async.Pipe.iter encoder_stream ~f:(fun encoder ->
        let payload = Grpc.Message.make encoder in
        H2.Body.Writer.write_string body payload;
        return ())
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
  let%bind () =
    Async.Pipe.iter encoder_stream ~f:(fun input ->
        let payload = Grpc.Message.make input in
        H2.Body.Writer.write_string body payload;
        H2.Body.Writer.flush body (fun () -> ());
        return ())
  in
  let%map status = Async.Mvar.take status_mvar in
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
