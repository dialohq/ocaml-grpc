module Net = Net

module Rpc = struct
  type stream = Grpc_core_eio.Stream.t
  type unary = string -> Grpc_server.trailers * string option
  type client_streaming = stream -> Grpc_server.trailers * string option
  type server_streaming = string -> (string -> unit) -> Grpc_server.trailers

  type bidirectional_streaming =
    stream -> (string -> unit) -> Grpc_server.trailers

  type rpc_impl = stream -> (string -> unit) -> Grpc_server.trailers

  type 'request handler = {
    headers : 'request -> Grpc_server.headers;
    f : stream -> (string -> unit) -> Grpc_server.trailers;
  }

  module Stream = Grpc_core_eio.Stream

  let unary (unary_handler : unary) =
   fun request_stream respond ->
    match Eio.Stream.take request_stream with
    | Some request ->
        let status, response = unary_handler request in
        Option.iter respond response;
        status
    | None -> Grpc_server.make_trailers Grpc.Status.(v OK)

  let client_streaming (client_streaming_handler : client_streaming) =
   fun request_stream respond ->
    let status, response = client_streaming_handler request_stream in
    Option.iter respond response;
    status

  let server_streaming (server_streaming_handler : server_streaming) =
   fun requests respond ->
    match Eio.Stream.take requests with
    | Some request -> server_streaming_handler request respond
    | None -> Grpc_server.make_trailers Grpc.Status.(v OK)
end

module Service = Grpc_server.Service
module G = Grpc_server

type 'request t = 'request Rpc.handler Grpc_server.t

let make = G.v
let add_service = G.add_service

type 'request net = (module Net.S with type Request.t = 'request)

let handle_request (type request) ~(net : request net) server request =
  let module Net' = (val net) in
  match
    G.handle_request server
      ~is_post_request:(Net'.Request.is_post request)
      ~get_header:(fun header -> Net'.Request.get_header request header)
      ~path:(Net'.Request.target request)
  with
  | Ok handler ->
      let request_stream = Net'.Request.read_body request in
      let { Net.on_msg; write_trailers; close } =
        let headers = handler.Rpc.headers request in
        Net'.respond_streaming ~headers request
      in
      let trailers =
        handler.f request_stream (fun input -> on_msg (Grpc.Message.make input))
      in
      write_trailers trailers;
      close ()
  | Error e -> Net'.respond_error request e
