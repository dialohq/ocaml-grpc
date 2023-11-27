module type S = Ocaml_protoc_plugin.Service.Rpc

val make :
  (module Ocaml_protoc_plugin.Service.Rpc
     with type Request.t = 'request
      and type Response.t = 'response) ->
  ('request, 'response) Grpc_eio.Rpc_codec.t
