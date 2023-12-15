open Pbrt_services.Value_mode

module Call : sig
  val unary :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    ('request, unary, 'response, unary) Pbrt_services.Client.rpc ->
    do_request:Grpc_eio.Client.do_request ->
    'request ->
    f:('response option -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val client_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    ('request, stream, 'response, unary) Pbrt_services.Client.rpc ->
    do_request:Grpc_eio.Client.do_request ->
    f:('request Grpc_eio.Seq.writer -> 'response option Eio.Promise.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val server_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    ('request, unary, 'response, stream) Pbrt_services.Client.rpc ->
    do_request:Grpc_eio.Client.do_request ->
    'request ->
    f:('response Grpc_eio.Seq.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val bidirectional_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    ('request, stream, 'response, stream) Pbrt_services.Client.rpc ->
    do_request:Grpc_eio.Client.do_request ->
    f:('request Grpc_eio.Seq.writer -> 'response Grpc_eio.Seq.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result
end

module Implement : sig
  type rpc = unit Grpc_eio.Server.Typed_rpc.t

  val unary :
    ('request, unary, 'response, unary) Pbrt_services.Server.rpc ->
    f:('request -> Grpc.Status.t * 'response option) ->
    rpc

  val client_streaming :
    ('request, stream, 'response, unary) Pbrt_services.Server.rpc ->
    f:('request Seq.t -> Grpc.Status.t * 'response option) ->
    rpc

  val server_streaming :
    ('request, unary, 'response, stream) Pbrt_services.Server.rpc ->
    f:('request -> ('response -> unit) -> Grpc.Status.t) ->
    rpc

  val bidirectional_streaming :
    ('request, stream, 'response, stream) Pbrt_services.Server.rpc ->
    f:('request Seq.t -> ('response -> unit) -> Grpc.Status.t) ->
    rpc

  val server : rpc Pbrt_services.Server.t -> Grpc_eio.Server.t
end
