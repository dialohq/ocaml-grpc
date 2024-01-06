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
