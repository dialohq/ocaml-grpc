(** A utility library for constructing gRPC specifications using [Ocaml_protoc].

    This module is designed to work alongside [Ocaml_protoc] to generate gRPC
    stubs, as outlined in {!module:Grpc.Rpc}. It offers a collection of helper
    functions that construct gRPC specifications from the code produced by
    [Ocaml_protoc] based on the services defined in *.proto files. *)

(** {1 Client side} *)

module Client_rpc : sig
  val unary :
    ( 'request,
      Pbrt_services.Value_mode.unary,
      'response,
      Pbrt_services.Value_mode.unary )
    Pbrt_services.Client.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary )
    Grpc.Rpc.Client_rpc.t

  val client_streaming :
    ( 'request,
      Pbrt_services.Value_mode.stream,
      'response,
      Pbrt_services.Value_mode.unary )
    Pbrt_services.Client.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary )
    Grpc.Rpc.Client_rpc.t

  val server_streaming :
    ( 'request,
      Pbrt_services.Value_mode.unary,
      'response,
      Pbrt_services.Value_mode.stream )
    Pbrt_services.Client.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream )
    Grpc.Rpc.Client_rpc.t

  val bidirectional_streaming :
    ( 'request,
      Pbrt_services.Value_mode.stream,
      'response,
      Pbrt_services.Value_mode.stream )
    Pbrt_services.Client.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream )
    Grpc.Rpc.Client_rpc.t
end

(** {1 Server side} *)

module Server_rpc : sig
  val unary :
    ( 'request,
      Pbrt_services.Value_mode.unary,
      'response,
      Pbrt_services.Value_mode.unary )
    Pbrt_services.Server.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary,
      unit )
    Grpc.Rpc.Server_rpc.t

  val client_streaming :
    ( 'request,
      Pbrt_services.Value_mode.stream,
      'response,
      Pbrt_services.Value_mode.unary )
    Pbrt_services.Server.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary,
      unit )
    Grpc.Rpc.Server_rpc.t

  val server_streaming :
    ( 'request,
      Pbrt_services.Value_mode.unary,
      'response,
      Pbrt_services.Value_mode.stream )
    Pbrt_services.Server.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream,
      unit )
    Grpc.Rpc.Server_rpc.t

  val bidirectional_streaming :
    ( 'request,
      Pbrt_services.Value_mode.stream,
      'response,
      Pbrt_services.Value_mode.stream )
    Pbrt_services.Server.rpc ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream,
      unit )
    Grpc.Rpc.Server_rpc.t
end

val handlers : 'a Pbrt_services.Server.t -> (_, 'a) Grpc.Rpc.Handlers.t
