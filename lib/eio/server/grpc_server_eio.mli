module Net = Net

module Rpc : sig
  type stream = Grpc_core_eio.Stream.t

  type 'request handler = {
    headers : 'request -> Grpc_server.headers;
    f : stream -> (string -> unit) -> Grpc_server.trailers;
  }

  type rpc_impl = stream -> (string -> unit) -> Grpc_server.trailers
  (** [handler] represents the most general signature of a gRPC handler. *)

  type unary = string -> Grpc_server.trailers * string option
  type client_streaming = stream -> Grpc_server.trailers * string option
  type server_streaming = string -> (string -> unit) -> Grpc_server.trailers
  type bidirectional_streaming = rpc_impl

  val unary : unary -> rpc_impl
  val client_streaming : client_streaming -> rpc_impl
  val server_streaming : server_streaming -> rpc_impl
end

module Service = Grpc_server.Service

type 'request t = 'request Rpc.handler Grpc_server.t

val make : unit -> 'a t

val add_service :
  name:string -> service:'a Rpc.handler Service.t -> 'a t -> 'a t

(* TODO: add context *)
val handle_request :
  net:'request Net.t -> 'request Rpc.handler Grpc_server.t -> 'request -> unit
