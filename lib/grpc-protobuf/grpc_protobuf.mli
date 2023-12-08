module type S = Ocaml_protoc_plugin.Service.Rpc

val rpc :
  (module Ocaml_protoc_plugin.Service.Rpc
     with type Request.t = 'request
      and type Response.t = 'response) ->
  ('request, 'response) Grpc.Rpc.t
