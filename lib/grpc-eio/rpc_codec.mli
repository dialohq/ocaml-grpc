module type Codec = Rpc_codec_interface.Codec
module type S = Rpc_codec_interface.S

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
