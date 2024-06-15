module Io = Io

exception Server_error of Grpc.Status.t * (string * string) list

type extra_trailers = (string * string) list

module Rpc : sig
  type rpc_complete

  type ('req, 'res) handler_accept = {
    accept :
      Grpc_server.headers ->
      ('req Seq.t -> ('res -> unit) -> extra_trailers) ->
      rpc_complete;
  }

  type ('net_req, 'req, 'res) handler =
    'net_req -> ('req, 'res) handler_accept -> rpc_complete

  type ('req, 'res) rpc_impl = 'req Seq.t -> ('res -> unit) -> extra_trailers
  (** [handler] represents the most general signature of a gRPC handler. *)

  type ('req, 'res) unary = 'req -> 'res * extra_trailers
  type ('req, 'res) client_streaming = 'req Seq.t -> 'res * extra_trailers
  type ('req, 'res) server_streaming = 'req -> ('res -> unit) -> extra_trailers
  type ('req, 'res) bidirectional_streaming = ('req, 'res) rpc_impl

  val unary : ('req, 'res) unary -> ('req, 'res) rpc_impl
  val client_streaming : ('req, 'res) client_streaming -> ('req, 'res) rpc_impl
  val server_streaming : ('req, 'res) server_streaming -> ('req, 'res) rpc_impl
end

type ('net_request, 'req, 'resp) t =
  service:string -> meth:string -> ('net_request, 'req, 'resp) Rpc.handler

val handle_request :
  ?error_handler:(exn -> extra_trailers) ->
  io:('net_request, 'req, 'resp) Io.t ->
  ('net_request, 'req, 'resp) t ->
  'net_request ->
  unit
