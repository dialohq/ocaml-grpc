module type S = Ocaml_protoc_plugin.Service.Rpc

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
