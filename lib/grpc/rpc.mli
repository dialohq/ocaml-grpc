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

val service_name : _ t -> string
val rpc_name : _ t -> string

module Codec : sig
  type 'a t = (module Codec with type t = 'a)
end

val request : ('request, _) t -> 'request Codec.t
val response : (_, 'response) t -> 'response Codec.t
val encode : 'a Codec.t -> 'a -> string
val decode : 'a Codec.t -> string -> 'a
