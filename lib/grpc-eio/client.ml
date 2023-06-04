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
    let send, close = Connection.grpc_send_streaming_client write_body in
    let response_reader = Connection.grpc_recv_streaming read_body in
    f ~send ~close response_reader

  let client_streaming ~f =
    bidirectional_streaming ~f:(fun ~send ~close responses ->
        let response = Stream.take responses in
        f ~send ~close response)

  let server_streaming ~f request =
    bidirectional_streaming ~f:(fun ~send ~close responses ->
        send request;
        close ();
        f responses)

  let unary ~f request =
    bidirectional_streaming ~f:(fun ~send ~close responses ->
        send request;
        close ();
        let response = Stream.take responses |> Eio.Promise.await in
        f response)
end
