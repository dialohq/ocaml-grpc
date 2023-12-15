module type S = Ocaml_protoc_plugin.Service.Rpc

module Call : sig
  val unary :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    do_request:Grpc_eio.Client.do_request ->
    'request ->
    f:('response option -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val client_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    do_request:Grpc_eio.Client.do_request ->
    f:('request Grpc_eio.Seq.writer -> 'response option Eio.Promise.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val server_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    do_request:Grpc_eio.Client.do_request ->
    'request ->
    f:('response Grpc_eio.Seq.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result

  val bidirectional_streaming :
    ?scheme:string ->
    ?headers:H2.Headers.t ->
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    do_request:Grpc_eio.Client.do_request ->
    f:('request Grpc_eio.Seq.writer -> 'response Grpc_eio.Seq.t -> 'a) ->
    ('a * Grpc.Status.t, H2.Status.t) result
end

module Implement : sig
  type rpc = Grpc.Rpc.Service_spec.t Grpc_eio.Server.Typed_rpc.t

  val unary :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    f:('request -> Grpc.Status.t * 'response option) ->
    rpc

  val client_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    f:('request Seq.t -> Grpc.Status.t * 'response option) ->
    rpc

  val server_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    f:('request -> ('response -> unit) -> Grpc.Status.t) ->
    rpc

  val bidirectional_streaming :
    (module S with type Request.t = 'request and type Response.t = 'response) ->
    f:('request Seq.t -> ('response -> unit) -> Grpc.Status.t) ->
    rpc

  val server : rpc list -> Grpc_eio.Server.t
end
