type t = { mutable contents : bytes; mutable length : int }
(** [t] represents a safer [bytes] in that the length is different from the capacity *)

let v ?(capacity = 2048) () = { contents = Bytes.create capacity; length = 0 }

let length t = t.length

let capacity t = Bytes.length t.contents

let extend t amount =
  t.contents <- Bytes.extend t.contents 0 amount;
  t.length <- t.length + amount

let log2 i = log (float_of_int i) /. log 2.

let ensure_size t len =
  if t.length + len >= capacity t then
    extend t (int_of_float (log2 (t.length + len)) - capacity t)

let copy_from_bigstringaf ~src_off ~src ~dst ~len =
  ensure_size dst len;
  Bigstringaf.blit_to_bytes ~src_off src ~dst_off:dst.length dst.contents ~len;
  dst.length <- dst.length + len

let sub t ~start ~length =
  let contents = Bytes.sub t.contents start length in
  { contents; length }

let to_bytes t = Bytes.sub t.contents 0 t.length

let get_uint8 t ~pos = Bytes.get_uint8 t.contents pos

let shift_left t ~by =
  Bytes.blit t.contents by t.contents 0 (t.length - by);
  t.length <- t.length - by

let unpack_unsigned_32_int_big_endian t ~pos =
  Binary_packing.unpack_unsigned_32_int_big_endian ~buf:t.contents ~pos
