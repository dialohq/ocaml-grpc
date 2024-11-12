exception Unexpected_eof
exception Connection_error of H2.Reqd.error

module Io = struct
  type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
  type response = Pbrt.Encoder.t -> unit

  module Growing_buffer = Grpc.Buffer

  module Net_request = struct
    type t =
      Eio.Net.Sockaddr.stream
      * H2.Reqd.t
      * H2.Request.t
      * H2.Reqd.error Eio.Promise.t

    let is_post (_, _, req, _) =
      match req with { H2.Request.meth = `POST; _ } -> true | _ -> false

    let target (_, _, req, _) = req.H2.Request.target

    (* Expose a way to interrupt *)
    let get_header (_, _, req, _) name =
      H2.Headers.get req.H2.Request.headers name

    let to_seq recv =
      let rec loop recv () =
        match recv () with
        | Grpc_eio_core.Recv_seq.Done -> Seq.Nil
        | Next (x, recv) -> Seq.Cons (x, loop recv)
        | Err `Unexpected_eof -> raise Unexpected_eof
        | Err (`Connection_error error) -> raise (Connection_error error)
      in
      loop recv

    let body (_, reqd, _, error) =
      let body = H2.Reqd.request_body reqd in
      (fun () ->
        Grpc_eio_core.Body_reader.read_next ~error
          (H2.Body.Reader.schedule_read body))
      |> Grpc_eio_core.Recv_seq.map
           (fun { Grpc_eio_core.Body_reader.consume } ->
             {
               Grpc_eio_core.Body_reader.consume =
                 (fun f ->
                   consume (fun { Grpc_eio_core.Body_reader.bytes; len } ->
                       f (Pbrt.Decoder.of_subbytes bytes 0 len)));
             })
      |> to_seq
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

  let respond_streaming ~headers (_, reqd, _, _) =
    let body_writer =
      H2.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd
        (H2.Response.create
           ~headers:
             (H2.Headers.of_list
                (("content-type", headers.Grpc_server.content_type)
                :: headers.extra))
           `OK)
    in
    let encoder = Pbrt.Encoder.create () in
    let close () = H2.Body.Writer.close body_writer in
    let header_buffer = Bytes.create 5 in
    let write input =
      Pbrt.Encoder.clear encoder;
      input encoder;
      let data = Pbrt.Encoder.to_bytes encoder |> Bytes.unsafe_to_string in
      Grpc.Message.fill_header ~length:(String.length data) header_buffer;
      H2.Body.Writer.write_string body_writer
        (Bytes.unsafe_to_string header_buffer);
      H2.Body.Writer.write_string body_writer data;
      H2.Body.Writer.flush body_writer ignore
    in
    let write_trailers = write_trailers reqd in
    let is_closed () = H2.Body.Writer.is_closed body_writer in
    { Grpc_server_eio.Io.close; write; write_trailers; is_closed }

  let respond_error ~status_code ~headers (_, reqd, _, _) =
    H2.Reqd.respond_with_string reqd
      (H2.Response.create
         ~headers:(H2.Headers.of_list headers)
         (H2.Status.of_code status_code))
      ""
end

include Io

let io =
  (module Io : Grpc_server_eio.Io.S
    with type Net_request.t = Io.Net_request.t
     and type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit)

let connection_handler ~sw ?config ?h2_error_handler ?grpc_error_handler server
    : 'a Eio.Net.connection_handler =
 fun socket addr ->
  let error, error_r = Eio.Promise.create () in
  let error_handler client_address ?request error respond =
    (* Report internal error via headers *)
    Eio.Promise.resolve error_r error;
    let () =
      match h2_error_handler with
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
          Grpc_server_eio.handle_request ~io ?error_handler:grpc_error_handler
            server
            (client_addr, reqd, H2.Reqd.request reqd, error)))
    ~error_handler addr socket ~sw
