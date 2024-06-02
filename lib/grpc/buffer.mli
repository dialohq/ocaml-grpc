type t
(** [t] represents a buffer which is based on bytes but keeps track of the
    length of valid data inside it. The internal capacity is increased when
    needed. *)

val v : ?capacity:int -> unit -> t
(** [c ~capacity ()] creates a new buffer with internal capacity [capacity]. *)

val length : t -> int
(** [length t] returns the length of valid data in the buffer. *)

val capacity : t -> int
(** [capacity t] returns the total capacity of the buffer. *)

val copy_from_bigstringaf :
  src_off:int -> src:Bigstringaf.t -> dst:t -> length:int -> unit
(** [copy_from_bigstringaf ~src_off ~src ~dst ~length] copies data from [src]
    into [dst] starting from [src_off] and ending at [src_off + length] to the
    end of the buffer. *)

val sub_string : start:int -> length:int -> t -> string
(** [sub_string ~start ~length t] returns a string containing the data in the
    range \[start, start+length). *)

val get_u8 : pos:int -> t -> int
(** [get_u8 ~pos t] returns the unsigned 8 bit integer at [pos] in [t]. *)

val get_u32_be : pos:int -> t -> int
(** [get_u32_be ~pos t] returns the unsigned 32 bit big endian integer at [pos]
    in [t]. *)

val shift_left : by:int -> t -> unit
(** [shift_left ~by t] shifts [t] left by [by] positions, discarding the data. *)

val internal_buffer : t -> Bytes.t
(** [internal_buffer t] returns the internal buffer. *)
