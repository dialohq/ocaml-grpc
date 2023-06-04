include Grpc.Server.S

module Rpc : sig
  type unary = string -> Grpc.Status.t * string option
  (** [unary] is the type for a unary grpc rpc, one request, one response. *)

  type client_streaming = Stream.t -> Grpc.Status.t * string option
  (** [client_streaming] is the type for an rpc where the client streams the requests and the server responds once. *)

  type server_streaming = string -> (string -> unit) -> Grpc.Status.t
  (** [server_streaming] is the type for an rpc where the client sends one request and the server sends multiple responses. *)

  type bidirectional_streaming = Stream.t -> (string -> unit) -> Grpc.Status.t
  (** [bidirectional_streaming] is the type for an rpc where both the client and server can send multiple messages. *)

  type t =
    | Unary of unary
    | Client_streaming of client_streaming
    | Server_streaming of server_streaming
    | Bidirectional_streaming of bidirectional_streaming

  (** [t] represents the types of rpcs available in gRPC. *)

  val unary : f:unary -> H2.Reqd.t -> unit
  (** [unary ~f reqd] calls [f] with the request obtained from [reqd] and handles sending the response. *)

  val client_streaming : f:client_streaming -> H2.Reqd.t -> unit
  (** [client_streaming ~f reqd] calls [f] with a stream to pull requests from and handles sending the response. *)

  val server_streaming : f:server_streaming -> H2.Reqd.t -> unit
  (** [server_streaming ~f reqd] calls [f] with the request optained from [reqd] and handles sending the responses pushed out. *)

  val bidirectional_streaming : f:bidirectional_streaming -> H2.Reqd.t -> unit
  (** [bidirectional_streaming ~f reqd] calls [f] with a stream to pull requests from and andles sending the responses pushed out. *)
end

module Service : sig
  type t
  (** [t] represents a gRPC service with potentially multiple rpcs and the information needed to route to them. *)

  val v : unit -> t
  (** [v ()] creates a new service *)

  val add_rpc : name:string -> rpc:Rpc.t -> t -> t
  (** [add_rpc ~name ~rpc t] adds [rpc] to [t] and ensures that [t] can route to it with [name]. *)

  val handle_request : t -> H2.Reqd.t -> unit
  (** [handle_request t reqd] handles routing [reqd] to the correct rpc if available in [t]. *)
end
