open Ocaml_protoc_compiler_lib

val gen_service_server_struct :
  proto_gen_module:string ->
  Pb_codegen_ocaml_type.service ->
  Pb_codegen_formatting.scope ->
  unit

val gen_service_client_struct :
  proto_gen_module:string ->
  Pb_codegen_ocaml_type.service ->
  Pb_codegen_formatting.scope ->
  unit
