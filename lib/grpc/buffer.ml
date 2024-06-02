type t = { mutable contents : bytes; mutable length : int }

let v ?(capacity = 1024) () = { contents = Bytes.create capacity; length = 0 }
let length t = t.length
let capacity t = Bytes.length t.contents
let extend t amount = t.contents <- Bytes.extend t.contents 0 amount

let rec nearest_power_of_2 acc target =
  if acc >= target then acc else nearest_power_of_2 (acc * 2) target

let ensure_size t ~extra =
  let current_capacity = capacity t in
  let needed_capacity = t.length + extra in
  if needed_capacity >= current_capacity then
    extend t
      (nearest_power_of_2 current_capacity needed_capacity - current_capacity)

let copy_from_bigstringaf ~src_off ~src ~dst ~length =
  ensure_size dst ~extra:length;
  Bigstringaf.blit_to_bytes ~src_off src ~dst_off:dst.length dst.contents
    ~len:length;
  dst.length <- dst.length + length

let sub_string ~start ~length t = Bytes.sub_string t.contents start length

let shift_left ~by t =
  Bytes.blit t.contents by t.contents 0 (t.length - by);
  t.length <- t.length - by

let get_u8 ~pos t = Bytes.get_uint8 t.contents pos

let get_u32_be ~pos t =
  let high = Bytes.get_uint16_be t.contents pos in
  let low = Bytes.get_uint16_be t.contents (pos + 2) in
  (high lsl 16) lor low

let internal_buffer t = t.contents
