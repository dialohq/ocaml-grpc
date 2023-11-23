module type S = Ocaml_protoc_plugin.Service.Rpc

type ('request, 'response) t =
  (module S with type Request.t = 'request and type Response.t = 'response)

val service_name : _ t -> string
val rpc_name : _ t -> string

val encode :
  (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message with type t = 'a) ->
  'a ->
  string

val decode_exn :
  (module Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message with type t = 'a) ->
  string ->
  'a
