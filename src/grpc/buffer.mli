type t

val v : ?capacity:int -> unit -> t

val length : t -> int

val capacity : t -> int

val to_bytes : t -> bytes

val copy_from_bigstringaf :
  src_off:int -> src:Bigstringaf.t -> dst:t -> length:int -> unit

val sub : start:int -> length:int -> t -> t

val get_u8 : pos:int -> t -> int

val get_u32_be : pos:int -> t -> int

val shift_left : by:int -> t -> unit
