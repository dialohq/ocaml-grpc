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

let service_spec (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  {
    Grpc.Rpc.Service_spec.package = R.package_name |> Option.to_list;
    service_name = R.service_name;
  }

let client_rpc (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  {
    Grpc.Rpc.Client_rpc.service_spec = service_spec (module R);
    rpc_name = R.method_name;
    encode_request = encode (module R.Request);
    decode_response = decode (module R.Response);
  }

let server_rpc (type request response)
    (module R : S with type Request.t = request and type Response.t = response)
    =
  {
    Grpc.Rpc.Server_rpc.service_spec = Some (service_spec (module R));
    rpc_name = R.method_name;
    decode_request = decode (module R.Request);
    encode_response = encode (module R.Response);
  }
