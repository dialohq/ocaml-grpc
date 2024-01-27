(** A utility library for constructing gRPC specifications using
    [Ocaml_protoc_plugin].

    This module is designed to work alongside [Ocaml_protoc_plugin] to generate
    gRPC stubs, as outlined in {!module:Grpc.Rpc}. It offers a collection of
    helper functions that construct gRPC specifications from the code produced
    by [Ocaml_protoc_plugin] based on the services defined in *.proto files. *)

module type S = Ocaml_protoc_plugin.Service.Rpc
(** For each service delineated in *.proto files, [Ocaml_protoc_plugin]
    generates a module that conforms to the type interface [S]. This module
    serves as the entry point for this library to create the corresponding
    gRPC specifications on the client and server sides. It is to be supplied
    to the corresponding helper as a first class module parameter. *)

(** {1 Client side} *)

module Client_rpc : sig
  val unary :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary )
    Grpc.Rpc.Client_rpc.t

  val client_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary )
    Grpc.Rpc.Client_rpc.t

  val server_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream )
    Grpc.Rpc.Client_rpc.t

  val bidirectional_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream )
    Grpc.Rpc.Client_rpc.t
end

(** {1 Server side} *)

module Server_rpc : sig
  val unary :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary,
      Grpc.Rpc.Service_spec.t )
    Grpc.Rpc.Server_rpc.t

  val client_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary,
      Grpc.Rpc.Service_spec.t )
    Grpc.Rpc.Server_rpc.t

  val server_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream,
      Grpc.Rpc.Service_spec.t )
    Grpc.Rpc.Server_rpc.t

  val bidirectional_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream,
      Grpc.Rpc.Service_spec.t )
    Grpc.Rpc.Server_rpc.t
end

val handlers : 'a list -> ('a, _) Grpc.Rpc.Handlers.t
