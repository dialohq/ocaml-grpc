include Rpc_codec_interface

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

module Codec = struct
  type 'a t = (module Codec with type t = 'a)
end

let request (type request response)
    (module Rpc_codec : S
      with type Request.t = request
       and type Response.t = response) =
  (module Rpc_codec.Request : Codec with type t = request)

let response (type request response)
    (module Rpc_codec : S
      with type Request.t = request
       and type Response.t = response) =
  (module Rpc_codec.Response : Codec with type t = response)

let encode (type a) (module M : Codec with type t = a) (a : a) = a |> M.encode

let decode (type a) (module M : Codec with type t = a) buffer : a =
  buffer |> M.decode
