type t = { mutable contents : bytes; mutable length : int }

let v ?(capacity = 2048) () = { contents = Bytes.create capacity; length = 0 }

let length t = t.length

let capacity t = Bytes.length t.contents

let extend t amount =
  t.contents <- Bytes.extend t.contents 0 amount;
  t.length <- t.length + amount

let log2 i = log (float_of_int i) /. log 2.

let ensure_size t ~extra =
  if t.length + extra >= capacity t then
    extend t (int_of_float (log2 (t.length + extra)) - capacity t)

let copy_from_bigstringaf ~src_off ~src ~dst ~length =
  ensure_size dst ~extra:length;
  Bigstringaf.blit_to_bytes ~src_off src ~dst_off:dst.length dst.contents
    ~len:length;
  dst.length <- dst.length + length

let sub ~start ~length t =
  let contents = Bytes.sub t.contents start length in
  { contents; length }

let to_bytes t = Bytes.sub t.contents 0 t.length

let shift_left ~by t =
  Bytes.blit t.contents by t.contents 0 (t.length - by);
  t.length <- t.length - by

let get_u8 ~pos t = Bytes.get_uint8 t.contents pos

let get_u32_be ~pos t =
  let high = Bytes.get_uint16_be t.contents pos in
  let low = Bytes.get_uint16_be t.contents (pos + 2) in
  (high lsl 16) lor low
