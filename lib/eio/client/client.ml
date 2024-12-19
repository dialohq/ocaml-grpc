type ('net_response, 'response, 'stream_err, 'headers) recv = {
  net_response : 'net_response;
  recv_seq : ('response, 'stream_err) Grpc_eio_core.Recv_seq.t;
  trailers : 'headers Eio.Promise.t;
}

type 'request writer = {
  write : 'request -> bool;
  (* Returns true if the write was successful, false if the stream is in error state. Throws if the stream was closed. *)
  close : unit -> unit;
}

type ('net_response,
       'headers,
       'request,
       'response,
       'conn_error,
       'stream_error)
     connection = {
  writer : 'request writer;
  recv :
    ( ('net_response, 'response, 'stream_error, 'headers) recv,
      'conn_error )
    result
    Eio.Promise.t;
  grpc_status : Grpc.Status.t Eio.Promise.t;
  write_exn : exn option ref;
  conn_err : 'conn_error Eio.Promise.t;
}

let call (type headers net_response request response stream_error conn_error)
    ~sw
    ~(io :
       (headers, net_response, request, response, stream_error, conn_error) Io.t)
    ~service ~method_name ~(headers : Grpc_client.request_headers) () :
    ( ( net_response,
        headers,
        request,
        response,
        conn_error,
        stream_error )
      connection,
      conn_error )
    result =
  let (module Io') = io in
  let path = Grpc_client.make_path ~service ~method_name in
  let writer', recv_net, conn_err_p = Io'.send_request ~headers path in
  let write_exn = ref None in
  let writer =
    {
      write =
        (fun req ->
          try
            writer'.write req;
            true
          with exn ->
            write_exn := Some exn;
            false);
      close = writer'.close;
    }
  in
  let status, status_notify = Eio.Promise.create () in
  let recv, recv_notify = Eio.Promise.create () in
  let () =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Promise.resolve recv_notify
          (match Eio.Promise.await recv_net with
          | Error conn_error ->
              Eio.Promise.resolve status_notify
                (Grpc.Status.make ~error_message:"Connection error"
                   Grpc.Status.Unknown);
              Error conn_error
          | Ok { response; next; trailers } ->
              Eio.Fiber.fork_daemon ~sw (fun () ->
                  Eio.Promise.resolve status_notify
                    (Grpc_client.status_of_trailers
                       ~get_header:
                         (Io'.Headers.get (Eio.Promise.await trailers)));
                  `Stop_daemon);
              Ok { net_response = response; recv_seq = next; trailers });
        `Stop_daemon)
  in
  Ok { writer; recv; grpc_status = status; write_exn; conn_err = conn_err_p }

type ('a, 'headers) streaming_result_success = {
  result : 'a;
  trailers : 'headers;
}

module Bidirectional_streaming = struct
  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Stream_result_success of ('a, 'headers) streaming_result_success
    | `Stream_result_error of
      ('a, 'headers, 'stream_err) Rpc_error.streaming_result_err
    | ('net_response, 'headers, 'conn_err) Rpc_error.common_error ]

  let call (type headers net_response request response stream_error conn_error)
      ~sw
      ~(io :
         ( headers,
           net_response,
           request,
           response,
           stream_error,
           conn_error )
         Io.t) ~service ~method_name ~headers f :
      (_, headers, stream_error, conn_error, net_response) result' =
    match call ~sw ~io ~service ~method_name ~headers () with
    | Ok { writer; recv; grpc_status; write_exn; conn_err = conn_err_p } -> (
        let closed = ref false in
        let writer =
          {
            write =
              (fun req ->
                let result = writer.write req in
                result);
            close =
              (fun () ->
                writer.close ();
                closed := true);
          }
        in
        let error = ref None in
        let res =
          Eio.Fiber.first
            (fun () ->
              Ok
                (f
                   (fun () ->
                     match Eio.Promise.await recv with
                     | Ok { net_response; _ } -> Ok net_response
                     | Error e -> Error e)
                   ~writer
                   ~read:(fun () ->
                     match Eio.Promise.await recv with
                     | Ok { net_response; recv_seq; _ } ->
                         let (module Io') = io in
                         if Io'.Net_response.is_ok net_response then
                           let rec read recv_seq' () =
                             match recv_seq' () with
                             | Grpc_eio_core.Recv_seq.Done -> Seq.Nil
                             | Err e ->
                                 let () = error := Some e in
                                 Seq.Nil
                             | Next (t, next) ->
                                 Seq.Cons (t, fun () -> read next ())
                           in
                           read recv_seq ()
                         else Seq.Nil
                     | Error _ -> Seq.Nil)))
            (fun () ->
              let erra = Eio.Promise.await conn_err_p in
              Error erra)
        in

        match res with
        | Error erra -> `Connection_error erra
        | Ok res -> (
            (* TODO: change await to peek to avoid deadlocking in case we never got response *)
            match Eio.Promise.peek recv with
            | None -> (* TODO: return something like `Cancelled *) Obj.magic ()
            | Some (Error e) -> `Connection_error e
            | Some (Ok { net_response = _; trailers; _ }) -> (
                if not !closed then writer.close ();
                match !error with
                | Some error ->
                    `Stream_result_error
                      {
                        result = res;
                        trailers = Eio.Promise.await trailers;
                        err =
                          {
                            stream_error = Some error;
                            grpc_status = Eio.Promise.await grpc_status;
                            write_exn = !write_exn;
                          };
                      }
                | None -> (
                    let status = Eio.Promise.await grpc_status in
                    match Grpc.Status.code status with
                    | Grpc.Status.OK -> (
                        match !write_exn with
                        | None ->
                            `Stream_result_success
                              {
                                result = res;
                                trailers = Eio.Promise.await trailers;
                              }
                        | Some _ ->
                            `Stream_result_error
                              {
                                result = res;
                                trailers = Eio.Promise.await trailers;
                                err =
                                  {
                                    write_exn = !write_exn;
                                    grpc_status = Eio.Promise.await grpc_status;
                                    stream_error = None;
                                  };
                              })
                    | _ ->
                        `Stream_result_error
                          {
                            result = res;
                            trailers = Eio.Promise.await trailers;
                            err =
                              {
                                grpc_status = status;
                                stream_error = None;
                                write_exn = !write_exn;
                              };
                          }))))
    | Error e -> `Connection_error e
end

module Unary = struct
  type ('net_response, 'response, 'headers) success = {
    net_response : 'net_response;
    response : 'response;
    trailers : 'headers;
  }

  type ('response, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Success of ('net_response, 'response, 'headers) success
    | `Premature_close of
      ('net_response, 'headers, 'stream_err) Rpc_error.Unary.premature_close
    | `Response_not_ok of ('net_response, 'headers) Rpc_error.resp_not_ok
    | `Connection_error of 'conn_err
    | `Write_error of exn ]

  let call (type headers net_response request response stream_error conn_error)
      ~sw
      ~(io :
         ( headers,
           net_response,
           request,
           response,
           stream_error,
           conn_error )
         Io.t) ~service ~method_name ~headers request :
      (_, headers, stream_error, conn_error, net_response) result' =
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
end

module Client_streaming = struct
  type ('a, 'response, 'headers) success = {
    result : 'a;
    response : 'response;
    trailers : 'headers;
    write_exn : exn option;
  }

  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response, 'response) result' =
    [ `Success of ('a, 'response, 'headers) success
    | `Premature_close of
      ('a, 'headers) Rpc_error.Client_streaming.premature_close
    | `Stream_error of
      ('a, 'headers, 'stream_err) Rpc_error.Client_streaming.stream_err
    | ('net_response, 'headers, 'conn_err) Rpc_error.common_error ]

  let call (type headers net_response request response stream_error conn_error)
      ~sw
      ~(io :
         ( headers,
           net_response,
           request,
           response,
           stream_error,
           conn_error )
         Io.t) ~service ~method_name ~headers f :
      (_, headers, stream_error, conn_error, net_response, response) result' =
    match call ~sw ~io ~service ~method_name ~headers () with
    | Ok { writer; recv; grpc_status; write_exn; conn_err = conn_err_p } -> (
        match Eio.Promise.await recv with
        | Error e -> `Connection_error e
        | Ok { net_response; recv_seq; trailers } ->
            let (module Io') = io in
            if Io'.Net_response.is_ok net_response then (
              let closed = ref false in
              let writer =
                {
                  write = writer.write;
                  close =
                    (fun () ->
                      writer.close ();
                      closed := true);
                }
              in

              let res =
                Eio.Fiber.first
                  (fun () -> Ok (f net_response ~writer))
                  (fun () ->
                    let erra = Eio.Promise.await conn_err_p in
                    Error erra)
              in
              if not !closed then writer.close ();

              match res with
              | Error erra -> `Connection_error erra
              | Ok res -> (
                  match recv_seq () with
                  | Grpc_eio_core.Recv_seq.Done ->
                      `Premature_close
                        {
                          result = res;
                          trailers = Eio.Promise.await trailers;
                          grpc_status = Eio.Promise.await grpc_status;
                          write_exn = !write_exn;
                        }
                  | Err e ->
                      `Stream_error
                        {
                          result = res;
                          stream_error = e;
                          trailers = Eio.Promise.await trailers;
                          grpc_status = Eio.Promise.await grpc_status;
                          write_exn = !write_exn;
                        }
                  | Next (t, _) -> (
                      let status = Eio.Promise.await grpc_status in
                      match Grpc.Status.code status with
                      | OK ->
                          `Success
                            {
                              result = res;
                              response = t;
                              trailers = Eio.Promise.await trailers;
                              write_exn = !write_exn;
                            }
                      | _ ->
                          `Response_not_ok
                            {
                              net_response;
                              grpc_status = status;
                              trailers = Eio.Promise.await trailers;
                            })))
            else
              `Response_not_ok
                {
                  net_response;
                  grpc_status = Eio.Promise.await grpc_status;
                  trailers = Eio.Promise.await trailers;
                })
    | Error e -> `Connection_error e
end

module Server_streaming = struct
  type ('a, 'headers, 'stream_error, 'net_response, 'conn_err) result' =
    [ `Stream_result_success of ('a, 'headers) streaming_result_success
    | `Stream_result_error of
      ('a, 'headers, 'stream_error) Rpc_error.streaming_result_err
    | `Write_error of
      ('stream_error, 'headers) Rpc_error.streaming_err option * 'headers
    | ('net_response, 'headers, 'conn_err) Rpc_error.common_error ]

  let call ~sw ~io ~service ~method_name ~headers request f =
    let result =
      Bidirectional_streaming.call ~sw ~io ~service ~method_name ~headers
        (fun net_response ~writer ~read ->
          if writer.write request then (
            writer.close ();
            `Stream (f net_response ~read))
          else `Write_error)
    in
    let module Bs = Bidirectional_streaming in
    match result with
    | #Rpc_error.common_error as common_err -> common_err
    | `Stream_result_success { result; trailers } -> (
        match result with
        | `Write_error -> `Write_error (None, trailers)
        | `Stream res -> `Stream_result_success { result = res; trailers })
    | `Stream_result_error { result; err; trailers } -> (
        match result with
        | `Write_error -> `Write_error (Some err, trailers)
        | `Stream res ->
            `Stream_result_error { Rpc_error.result = res; err; trailers })
end
