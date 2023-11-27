module type S = Ocaml_protoc_plugin.Service.Rpc

let encode (type a)
    (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
      with type t = a) (a : a) =
  a |> M.to_proto |> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.contents

let decode (type a)
    (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
      with type t = a) buffer =
  buffer |> Ocaml_protoc_plugin.Runtime.Runtime'.Reader.create |> M.from_proto
  |> function
  | Ok r -> r
  | Error e ->
      failwith
        (Printf.sprintf "Could not decode request: %s"
           (Ocaml_protoc_plugin.Result.show_error e))

let make (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  (module struct
    module Request = struct
      type t = request

      let encode t = encode (module R.Request) t
      let decode buffer = decode (module R.Request) buffer
    end

    module Response = struct
      type t = response

      let encode t = encode (module R.Response) t
      let decode buffer = decode (module R.Response) buffer
    end

    let package_name = R.package_name
    let service_name = R.service_name
    let method_name = R.method_name
  end : Grpc_eio.Rpc_codec.S
    with type Request.t = request
     and type Response.t = response)
