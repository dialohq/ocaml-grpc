(* 

  Possible causes of a gRPC request error:
  * HTTP/2 stream error - map h2 error code to gRPC error code and describe as transport STREAM error
  * HTTP/2 connection error - map h2 error code to gRPC error code and describe as transport CONNECTION error + debug message
  * HTTP/2 non-OK status - map h2 status code to gRPC error and describe as transport error "server return status code XXX"
  * gRPC non-OK status

*)

type 'msg response = ('msg, Status_new.t) result

let fill_header_cs ~length (buffer : Cstruct.t) =
  Cstruct.set_char buffer 0 '\x00';
  Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
  Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)

module Unary = struct
  let call ~sw:_ ~(channel : Channel.t) ~service ~method_name ~headers
      request_encoder : _ response =
    let pool = Grpc_eio_core.Buffer_pool.Bytes_pool.make () in

    (* sending request *)
    let header_buffer = Cstruct.create 5 in
    let body_buffer = Cstruct.create 100_000 in
    let encoder = Pbrt.Encoder.create ~size:65536 () in

    Pbrt.Encoder.clear encoder;
    request_encoder encoder;

    let length =
      Pbrt.Encoder.blit_to_buffer ~blit_from_bytes:Cstruct.blit_from_bytes
        encoder body_buffer 0
    in
    fill_header_cs ~length header_buffer;

    let css = [ header_buffer; Cstruct.sub body_buffer 0 length ] in
    let written = ref false in

    let path = Grpc_client.make_path ~service ~method_name in
    let data_writer : Channel.data_writer =
     fun () ->
      if !written then None
      else (
        written := true;
        Some css)
    in
    (* ****************** *)

    (* receiving response *)
    let response_promise, response_resolver = Eio.Promise.create () in

    let msg_state = ref Grpc_eio_core.Body_parse.Idle in
    let msg_stream = Eio.Stream.create Int.max_int in

    let data_receiver : Channel.data_receiver = function
      | Some { Cstruct.buffer = data; len; off } ->
          Grpc_eio_core.Body_parse.read_message ~pool ~data ~len ~off msg_stream
            msg_state
      | None ->
          let res = Eio.Stream.take msg_stream |> Option.get in
          Eio.Promise.resolve response_resolver
            {
              Grpc_eio_core.Body_parse.consume =
                (fun f ->
                  res.consume (fun { Grpc_eio_core.Body_parse.bytes; len } ->
                      f (Pbrt.Decoder.of_subbytes bytes 0 len)));
            }
    in
    (* ****************** *)

    let status_promise =
      Channel.start_request channel ~headers ~data_writer ~data_receiver ~path
    in

    (* Format.printf "Channel state: %a@." Channel.pp channel; *)
    match Eio.Promise.await status_promise with
    | { code = OK; _ } -> Ok (Eio.Promise.await response_promise)
    | status -> Error status

  (*
    match call ~sw ~io ~service ~method_name ~headers () with
    | Ok { writer; recv; grpc_status; write_exn; conn_err = conn_err_p } -> (
        try
          if not (writer.write request) then
            `Write_error (Option.get !write_exn)
          else (
            writer.close ();
            Eio.Fiber.first
              (fun () -> `Connection_error (Eio.Promise.await conn_err_p))
              (fun () ->
                match Eio.Promise.await recv with
                | Ok { net_response; recv_seq; trailers } ->
                    let (module Io') = io in
                    if Io'.Net_response.is_ok net_response then
                      match recv_seq () with
                      | Grpc_eio_core.Recv_seq.Done ->
                          (`Premature_close
                             {
                               net_response;
                               grpc_status = Eio.Promise.await grpc_status;
                               trailers = Eio.Promise.await trailers;
                               stream_error = None;
                             }
                            : ( response,
                                headers,
                                stream_error,
                                conn_error,
                                net_response )
                              result')
                      | Err stream_error ->
                          `Premature_close
                            {
                              net_response;
                              grpc_status = Eio.Promise.await grpc_status;
                              trailers = Eio.Promise.await trailers;
                              stream_error = Some stream_error;
                            }
                      | Next (response, _) -> (
                          let status = Eio.Promise.await grpc_status in
                          match Grpc.Status.code status with
                          | OK ->
                              `Success
                                {
                                  net_response;
                                  response;
                                  trailers = Eio.Promise.await trailers;
                                }
                          | _ ->
                              (* Not reachable under normal circumstances
                             https://github.com/grpc/grpc/issues/12824 *)
                              `Response_not_ok
                                {
                                  net_response;
                                  grpc_status = status;
                                  trailers = Eio.Promise.await trailers;
                                })
                    else
                      `Response_not_ok
                        {
                          net_response;
                          grpc_status = Eio.Promise.await grpc_status;
                          trailers = Eio.Promise.await trailers;
                        }
                | Error e -> `Connection_error e))
        with exn -> `Write_error exn)
    | Error e -> `Connection_error e
    *)
end
