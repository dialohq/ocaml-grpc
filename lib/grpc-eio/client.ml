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

module Rpc = struct
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t -> 'a

  let bidirectional_streaming ~f write_body read_body =
    let response_reader, response_writer = Seq.create_reader_writer () in
    let request_reader, request_writer = Seq.create_reader_writer () in
    Connection.grpc_recv_streaming read_body response_writer;
    let res, res_notify = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
        Eio.Promise.resolve res_notify (f request_writer response_reader))
      (fun () ->
        Connection.grpc_send_streaming_client write_body request_reader);
    Eio.Promise.await res

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
end

module Typed_rpc = struct
  type ('request, 'response, 'a) handler =
    ('request, 'response) Protoc_rpc.t ->
    H2.Body.Writer.t ->
    H2.Body.Reader.t ->
    'a

  let unary (type request response) ~f (request : request)
      (module R : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) =
    let request = Protoc_rpc.encode (module R.Request) request in
    let f response =
      let response =
        response
        |> Option.map (fun response ->
               response |> Protoc_rpc.decode_exn (module R.Response))
      in
      f response
    in
    Rpc.unary ~f request

  let server_streaming (type request response) ~f (request : request)
      (module R : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) =
    let request = Protoc_rpc.encode (module R.Request) request in
    let f responses =
      let responses =
        Seq.map
          (fun str -> Protoc_rpc.decode_exn (module R.Response) str)
          responses
      in
      f responses
    in
    Rpc.server_streaming ~f request

  let client_streaming (type request response) ~f
      (module R : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) =
    let f requests response =
      let requests_reader, requests' = Seq.create_reader_writer () in
      let response', response_u = Eio.Promise.create () in
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Fiber.both
            (fun () ->
              let response =
                Eio.Promise.await response
                |> Option.map (fun response ->
                       Protoc_rpc.decode_exn (module R.Response) response)
              in
              Eio.Promise.resolve response_u response)
            (fun () ->
              Seq.iter
                (fun request ->
                  Seq.write requests
                    (Protoc_rpc.encode (module R.Request) request))
                requests_reader;
              Seq.close_writer requests));
      f requests' response'
    in
    Rpc.client_streaming ~f

  let bidirectional_streaming (type request response) ~f
      (module R : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) =
    let f requests responses =
      let requests_reader, requests' = Seq.create_reader_writer () in
      let responses' =
        Seq.map
          (fun str -> Protoc_rpc.decode_exn (module R.Response) str)
          responses
      in
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.fork ~sw (fun () ->
          Seq.iter
            (fun request ->
              Seq.write requests (Protoc_rpc.encode (module R.Request) request))
            requests_reader;
          Seq.close_writer requests);
      f requests' responses'
    in
    Rpc.bidirectional_streaming ~f

  let call (type request response a)
      ((module R : Protoc_rpc.S
         with type Request.t = request
          and type Response.t = response) as protoc_rpc) ?scheme
      ~(handler : (request, response, a) handler) ~do_request ?headers () =
    call
      ~service:(Protoc_rpc.service_name protoc_rpc)
      ~rpc:(Protoc_rpc.rpc_name protoc_rpc)
      ?scheme ~handler:(handler protoc_rpc) ~do_request ?headers ()
end
