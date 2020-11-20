type t = { mutable contents : bytes; mutable length : int }
(** [t] represents a safer [bytes] in that the length is different from the capacity *)

let v ?(capacity = 2048) () = { contents = Bytes.create capacity; length = 0 }

let length t = t.length

let capacity t = Bytes.length t.contents

let extend t amount =
  t.contents <- Bytes.extend t.contents 0 amount;
  t.length <- t.length + amount

let log2 i = log (float_of_int i) /. log 2.

let ensure_size t ~length =
  if t.length + length >= capacity t then
    extend t (int_of_float (log2 (t.length + length)) - capacity t)

let copy_from_bigstringaf ~src_off ~src ~dst ~length =
  ensure_size dst ~length;
  Bigstringaf.blit_to_bytes ~src_off src ~dst_off:dst.length dst.contents
    ~len:length;
  dst.length <- dst.length + length

let sub t ~start ~length =
  let contents = Bytes.sub t.contents start length in
  { contents; length }

let to_bytes t = Bytes.sub t.contents 0 t.length

let get_u8 t ~pos = Bytes.get_uint8 t.contents pos

let shift_left t ~by =
  Bytes.blit t.contents by t.contents 0 (t.length - by);
  t.length <- t.length - by

let get_u32_be t ~pos =
  let high = Bytes.get_uint16_be t.contents pos in
  let low = Bytes.get_uint16_be t.contents (pos + 2) in
  (high lsl 16) lor low
