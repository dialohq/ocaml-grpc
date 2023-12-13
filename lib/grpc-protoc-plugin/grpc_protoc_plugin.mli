module type S = Ocaml_protoc_plugin.Service.Rpc

val client_rpc :
  (module Ocaml_protoc_plugin.Service.Rpc
     with type Request.t = 'request
      and type Response.t = 'response) ->
  ('request, 'response) Grpc.Rpc.Client_rpc.t

val server_rpc :
  (module Ocaml_protoc_plugin.Service.Rpc
     with type Request.t = 'request
      and type Response.t = 'response) ->
  ('request, 'response, Grpc.Rpc.Service_spec.t) Grpc.Rpc.Server_rpc.t
