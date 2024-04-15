module Net = struct
  module Request = struct
    type t = Eio.Net.Sockaddr.stream * H2.Reqd.t * H2.Request.t

    let is_post (_, _, req) =
      match req with { H2.Request.meth = `POST; _ } -> true | _ -> false

    let target (_, _, req) = req.H2.Request.target
    let get_header (_, _, req) name = H2.Headers.get req.H2.Request.headers name

    let read_body (_, reqd, _) =
      let body = H2.Reqd.request_body reqd in
      Grpc_core_eio.Stream.make
        ~schedule_read_raw:(H2.Body.Reader.schedule_read body)
  end

  let write_trailers reqd (trailers : Grpc_server.trailers) =
    try
      H2.Reqd.schedule_trailers reqd
        (H2.Headers.of_list
           (("grpc-status", string_of_int trailers.grpc_status)
           ::
           (match trailers.grpc_message with
           | None -> trailers.extra
           | Some msg -> ("grpc-message", msg) :: trailers.extra)))
    with
    | ((Failure "h2.Reqd.schedule_trailers: stream already closed")
    [@warning "-52"] (* https://github.com/anmonteiro/ocaml-h2/issues/175 *))
    ->
      ()

  let respond_streaming ~headers (_, reqd, _) =
    let body_writer =
      H2.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd
        (H2.Response.create
           ~headers:
             (H2.Headers.of_list
                (("content-type", headers.Grpc_server.content_type)
                :: headers.extra))
           `OK)
    in
    let close () = H2.Body.Writer.close body_writer in
    let on_msg input =

      H2.Body.Writer.write_string body_writer input
    in
    let write_trailers = write_trailers reqd in
    { Grpc_server_eio.Net.close; on_msg; write_trailers }

  let respond_error (_, reqd, _) (error : Grpc_server.error) =
    let respond_with code =
      H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
    in
    match error with
    | `Not_found _ -> respond_with `Not_found
    | `Unsupported_media_type -> respond_with `Unsupported_media_type
    | `Not_acceptable -> respond_with `Not_acceptable
    | `Bad_request -> respond_with `Bad_request
end

include Net

let net =
  (module Net : Grpc_server_eio.Net.S with type Request.t = Net.Request.t)

let connection_handler ~sw ?config ?error_handler server :
    'a Eio.Net.connection_handler =
 fun socket addr ->
  let error_handler client_address ?request error respond =
    (* Report internal error via headers *)
    let () =
      match error_handler with
      | Some f -> f client_address ?request error
      | None -> ()
    in
    let writer =
      respond
        (H2.Headers.of_list
           [
             ( "grpc-status",
               string_of_int (Grpc.Status.int_of_code Grpc.Status.Internal) );
           ])
    in
    H2.Body.Writer.close writer
  in
  H2_eio.Server.create_connection_handler ?config
    ~request_handler:(fun client_addr reqd ->
      Eio.Fiber.fork ~sw (fun () ->
          Grpc_server_eio.handle_request ~net server
            (client_addr, reqd, H2.Reqd.request reqd)))
    ~error_handler addr socket ~sw
