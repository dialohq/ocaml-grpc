module type S = Ocaml_protoc_plugin.Service.Rpc

type ('request, 'response) t =
  (module S with type Request.t = 'request and type Response.t = 'response)

let service_name (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  (match R.package_name with Some p -> p ^ "." | None -> "") ^ R.service_name

let rpc_name (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  R.method_name

let encode (type a)
    (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
      with type t = a) (a : a) =
  a |> M.to_proto |> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.contents

let decode_exn (type a)
    (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
      with type t = a) buffer =
  buffer |> Ocaml_protoc_plugin.Runtime.Runtime'.Reader.create |> M.from_proto
  |> function
  | Ok r -> r
  | Error e ->
      failwith
        (Printf.sprintf "Could not decode request: %s"
           (Ocaml_protoc_plugin.Result.show_error e))
