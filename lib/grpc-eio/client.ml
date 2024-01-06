type response_handler = H2.Client_connection.response_handler

type do_request =
  ?flush_headers_immediately:bool ->
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  H2.Body.Writer.t

let make_request ~scheme ~service ~rpc ~headers =
  H2.Request.create ~scheme `POST ("/" ^ service ^ "/" ^ rpc) ~headers

let default_headers =
  H2.Headers.of_list
    [ ("te", "trailers"); ("content-type", "application/grpc+proto") ]

let make_trailers_handler () =
  let status, status_notify = Eio.Promise.create () in
  let trailers_handler headers =
    let code =
      match H2.Headers.get headers "grpc-status" with
      | None -> None
      | Some s -> Option.bind (int_of_string_opt s) Grpc.Status.code_of_int
    in
    match (code, Eio.Promise.is_resolved status) with
    | Some code, false ->
        let message = H2.Headers.get headers "grpc-message" in
        let status = Grpc.Status.v ?message code in
        Eio.Promise.resolve status_notify status
    | Some _, true (* This should never happen, but just in case. *) | _ -> ()
  in
  (status, trailers_handler)

let get_response_and_bodies request =
  let response, response_notify = Eio.Promise.create () in
  let read_body, read_body_notify = Eio.Promise.create () in
  let response_handler response body =
    Eio.Promise.resolve response_notify response;
    Eio.Promise.resolve read_body_notify body
  in
  let write_body = request ~response_handler in
  let response = Eio.Promise.await response in
  let read_body = Eio.Promise.await read_body in
  (response, read_body, write_body)

let call ~service ~rpc ?(scheme = "https") ~handler ~(do_request : do_request)
    ?(headers = default_headers) () =
  let request = make_request ~service ~rpc ~scheme ~headers in
  let status, trailers_handler = make_trailers_handler () in
  let response, read_body, write_body =
    get_response_and_bodies
      (do_request ~flush_headers_immediately:true request ~trailers_handler)
  in
  match response.status with
  | `OK ->
      trailers_handler response.headers;
      let result = handler write_body read_body in
      let status =
        match Eio.Promise.is_resolved status with
        (* In case no grpc-status appears in headers or trailers. *)
        | true -> Eio.Promise.await status
        | false ->
            Grpc.Status.v ~message:"Server did not return grpc-status"
              Grpc.Status.Unknown
      in
      Ok (result, status)
  | error_status -> Error error_status

let make_handler ~encode_request ~decode_response ~f write_body read_body =
  let response_reader, response_writer = Seq.create_reader_writer () in
  let request_reader, request_writer = Seq.create_reader_writer () in
  Connection.Typed.grpc_recv_streaming ~decode:decode_response read_body
    response_writer;
  let res, res_notify = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () ->
      Eio.Promise.resolve res_notify (f request_writer response_reader))
    (fun () ->
      Connection.Typed.grpc_send_streaming_client ~encode:encode_request
        write_body request_reader);
  Eio.Promise.await res

module Typed_rpc = struct
  type ('request, 'request_mode, 'response, 'response_mode, 'a) handler =
    ('request, 'request_mode, 'response, 'response_mode) Grpc.Rpc.Client_rpc.t ->
    H2.Body.Writer.t ->
    H2.Body.Reader.t ->
    'a

  let make_handler (type request response)
      ~(rpc : (request, _, response, _) Grpc.Rpc.Client_rpc.t) ~f =
    make_handler ~encode_request:rpc.encode_request
      ~decode_response:rpc.decode_response ~f

  let bidirectional_streaming (type request response) ~f
      (rpc :
        ( request,
          Grpc.Rpc.Value_mode.stream,
          response,
          Grpc.Rpc.Value_mode.stream )
        Grpc.Rpc.Client_rpc.t) =
    make_handler ~rpc ~f

  let client_streaming (type request response) ~f
      (rpc :
        ( request,
          Grpc.Rpc.Value_mode.stream,
          response,
          Grpc.Rpc.Value_mode.unary )
        Grpc.Rpc.Client_rpc.t) =
    make_handler ~rpc ~f:(fun request_writer responses ->
        let response, response_resolver = Eio.Promise.create () in
        Eio.Fiber.pair
          (fun () -> f request_writer response)
          (fun () ->
            Eio.Promise.resolve response_resolver
              (Seq.read_and_exhaust responses))
        |> fst)

  let server_streaming (type request response) ~f (request : request)
      (rpc :
        ( request,
          Grpc.Rpc.Value_mode.unary,
          response,
          Grpc.Rpc.Value_mode.stream )
        Grpc.Rpc.Client_rpc.t) =
    make_handler ~rpc ~f:(fun request_writer responses ->
        Seq.write request_writer request;
        Seq.close_writer request_writer;
        f responses)

  let unary (type request response) ~f (request : request)
      (rpc :
        ( request,
          Grpc.Rpc.Value_mode.unary,
          response,
          Grpc.Rpc.Value_mode.unary )
        Grpc.Rpc.Client_rpc.t) =
    make_handler ~rpc ~f:(fun request_writer responses ->
        Seq.write request_writer request;
        Seq.close_writer request_writer;
        let response = Seq.read_and_exhaust responses in
        f response)

  let call (type request request_mode response response_mode a)
      (rpc :
        (request, request_mode, response, response_mode) Grpc.Rpc.Client_rpc.t)
      ?scheme
      ~(handler : (request, request_mode, response, response_mode, a) handler)
      ~do_request ?headers () =
    call
      ~service:(Grpc.Rpc.Service_spec.packaged_service_name rpc.service_spec)
      ~rpc:rpc.rpc_name ?scheme ~handler:(handler rpc) ~do_request ?headers ()
end

module Rpc = struct
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t -> 'a

  let bidirectional_streaming ~f =
    make_handler ~encode_request:Fun.id ~decode_response:Fun.id ~f

  let client_streaming ~f =
    bidirectional_streaming ~f:(fun request_writer responses ->
        let response, response_resolver = Eio.Promise.create () in
        Eio.Fiber.pair
          (fun () -> f request_writer response)
          (fun () ->
            Eio.Promise.resolve response_resolver
              (Seq.read_and_exhaust responses))
        |> fst)

  let server_streaming ~f request =
    bidirectional_streaming ~f:(fun request_writer responses ->
        Seq.write request_writer request;
        Seq.close_writer request_writer;
        f responses)

  let unary ~f request =
    bidirectional_streaming ~f:(fun request_writer responses ->
        Seq.write request_writer request;
        Seq.close_writer request_writer;
        let response = Seq.read_and_exhaust responses in
        f response)

  let call = call
end
