module type Buffer = sig
  type t

  val create : int -> t
  val len : t -> int
end

module MakePool (Buf : Buffer) : sig
  type buffer = Buf.t
  type t

  val make : unit -> t
  val alloc : t -> int -> buffer
  val release : t -> buffer -> unit
end

module Bigstring_pool : sig
  type buffer = Bigstringaf.t
  type t

  val make : unit -> t
  val alloc : t -> int -> buffer
  val release : t -> buffer -> unit
end

module Bytes_pool : sig
  type buffer = bytes
  type t

  val make : unit -> t
  val alloc : t -> int -> buffer
  val release : t -> buffer -> unit
end

module Cstruct_pool : sig
  type buffer = Cstruct.t
  type t

  val make : unit -> t
  val alloc : t -> int -> buffer
  val release : t -> buffer -> unit
end
