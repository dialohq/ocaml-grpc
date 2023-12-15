include Grpc.Server.S

module Rpc : sig
  type unary = string -> Grpc.Status.t * string option
  (** [unary] is the type for a unary grpc rpc, one request, one response. *)

  type client_streaming = string Seq.t -> Grpc.Status.t * string option
  (** [client_streaming] is the type for an rpc where the client streams the requests and the server responds once. *)

  type server_streaming = string -> (string -> unit) -> Grpc.Status.t
  (** [server_streaming] is the type for an rpc where the client sends one request and the server sends multiple responses. *)

  type bidirectional_streaming =
    string Seq.t -> (string -> unit) -> Grpc.Status.t
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

module Typed_rpc : sig
  (** This is an experimental API to build RPCs on the server side. Compared to
    {Rpc}, this interface will:

  - handle the coding/decoding of messages for you under the hood;
  - use the service and RPC names provided by the rpc specification to
    register the services with their expected names. *)

  type server := t

  type ('request, 'response) unary =
    'request -> Grpc.Status.t * 'response option
  (** [unary] is the type for a unary grpc rpc, one request, one response. *)

  type ('request, 'response) client_streaming =
    'request Seq.t -> Grpc.Status.t * 'response option
  (** [client_streaming] is the type for an rpc where the client streams the
      requests and the server responds once. *)

  type ('request, 'response) server_streaming =
    'request -> ('response -> unit) -> Grpc.Status.t
  (** [server_streaming] is the type for an rpc where the client sends one
    request and the server sends multiple responses. *)

  type ('request, 'response) bidirectional_streaming =
    'request Seq.t -> ('response -> unit) -> Grpc.Status.t
  (** [bidirectional_streaming] is the type for an rpc where both the client and
    server can send multiple messages. *)

  type 'service_spec t
  (** [t] represents an implementation for an RPC on the server side. *)

  (** The next functions are meant to be used by the server to create RPC
      implementations. The rpc specification that the function implements must
      be provided as it is used to handle coding/decoding of messages. It also
      allows to refer to the service and RPC names specified in the [.proto]
      file. *)

  val unary :
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary,
      'service_spec )
    Grpc.Rpc.Server_rpc.t ->
    f:('request, 'response) unary ->
    'service_spec t

  val client_streaming :
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary,
      'service_spec )
    Grpc.Rpc.Server_rpc.t ->
    f:('request, 'response) client_streaming ->
    'service_spec t

  val server_streaming :
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream,
      'service_spec )
    Grpc.Rpc.Server_rpc.t ->
    f:('request, 'response) server_streaming ->
    'service_spec t

  val bidirectional_streaming :
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream,
      'service_spec )
    Grpc.Rpc.Server_rpc.t ->
    f:('request, 'response) bidirectional_streaming ->
    'service_spec t

  val server : (Grpc.Rpc.Service_spec.t t, unit t) Grpc.Rpc.Handlers.t -> server
  (** Having built a list of RPCs you will use this function to package them up
      into a server that is ready to be served over the network. This function
      takes care of registering the services based on the names provided by the
      protoc specification.  *)
end
