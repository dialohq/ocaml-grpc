open Lwt.Syntax

let grpc_recv_streaming request decoder_push =
  let body = H2.Reqd.request_body request in
  let request_buffer = ref @@ Grpc.Buffer.v () in
  let rec on_read buffer ~off ~len =
    Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src:buffer
      ~dst:!request_buffer ~length:len;
    let message = Grpc.Message.extract !request_buffer in
    ( match message with
    | Some message ->
        decoder_push
          (Some (Pbrt.Decoder.of_bytes (Grpc.Buffer.to_bytes message)))
    | None -> () );
    H2.Body.schedule_read body ~on_read ~on_eof
  and on_eof () = decoder_push None in
  H2.Body.schedule_read body ~on_read ~on_eof

let grpc_send_streaming request encoder_stream status_mvar =
  let body =
    H2.Reqd.respond_with_streaming ~flush_headers_immediately:true request
      (H2.Response.create
         ~headers:
           (H2.Headers.of_list [ ("content-type", "application/grpc+proto") ])
         `OK)
  in
  let* () =
    Lwt_stream.iter
      (fun encoder ->
        let payload = Grpc.Message.make (Pbrt.Encoder.to_string encoder) in
        H2.Body.write_string body payload;
        H2.Body.flush body (fun () -> ()))
      encoder_stream
  in
  let+ status = Lwt_mvar.take status_mvar in
  H2.Reqd.schedule_trailers request
    (H2.Headers.of_list
       ( [
           ( "grpc-status",
             string_of_int (Grpc.Status.int_of_code (Grpc.Status.code status))
           );
         ]
       @
       match Grpc.Status.message status with
       | None -> []
       | Some message -> [ ("grpc-message", message) ] ));
  H2.Body.close_writer body
