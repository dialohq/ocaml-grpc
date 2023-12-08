type buffer = string

module type Codec = sig
  type t

  val encode : t -> buffer
  val decode : buffer -> t
end

module type S = sig
  module Request : sig
    type t

    include Codec with type t := t
  end

  module Response : sig
    type t

    include Codec with type t := t
  end

  val package_name : string option
  val service_name : string
  val method_name : string
end

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
    (module Rpc_spec : S
      with type Request.t = request
       and type Response.t = response) =
  (module Rpc_spec.Request : Codec with type t = request)

let response (type request response)
    (module Rpc_spec : S
      with type Request.t = request
       and type Response.t = response) =
  (module Rpc_spec.Response : Codec with type t = response)

let encode (type a) (module M : Codec with type t = a) (a : a) = a |> M.encode

let decode (type a) (module M : Codec with type t = a) buffer : a =
  buffer |> M.decode
