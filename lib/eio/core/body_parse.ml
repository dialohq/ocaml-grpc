open Buffer_pool

type t = { bytes : Bytes.t; len : int }
type 'a consumer = { consume : 'b. ('a -> 'b) -> 'b }

let take_buffer ~pool len = Bytes_pool.alloc pool len
let free_buffer ~pool bytes = Bytes_pool.release pool bytes

let to_consumer ~pool (t : t) =
  {
    consume =
      (fun f ->
        let res = f t in
        free_buffer ~pool t.bytes;
        res);
  }

type msg_state =
  | Idle
  | PartialHeader of Bigstringaf.t * int
  | PartialBody of Bytes.t * int * int

let pp_hum_msg_state fmt = function
  | Idle -> Format.fprintf fmt "Idle"
  | PartialHeader (bs, read) ->
      Format.fprintf fmt "PartialHeader (<bigstring %i>, %i)"
        (Bigstringaf.length bs) read
  | PartialBody (bytes, read, msg_len) ->
      Format.fprintf fmt "PartialBody (<bytes %i>, %i, %i)" (Bytes.length bytes)
        read msg_len

let extract_msg_len ~data ~off =
  let high = Bigstringaf.get_int16_be data off in
  let low = Bigstringaf.get_int16_be data (off + 2) in
  (high lsl 16) lor low

let rec read_message ~pool ~(data : Bigstringaf.t) ~len ~off msg_stream
    (state : msg_state ref) : unit =
  let take_buffer = take_buffer ~pool in
  let to_consumer = to_consumer ~pool in
  let read_message = read_message ~pool in

  match !state with
  | Idle ->
      if len >= 5 then (
        let msg_len = extract_msg_len ~data ~off:(off + 1) in
        if len - 5 >= msg_len then (
          let msg_buffer = take_buffer msg_len in

          Bigstringaf.blit_to_bytes data ~src_off:(off + 5) msg_buffer
            ~dst_off:0 ~len:msg_len;

          Eio.Stream.add msg_stream
            (Some (to_consumer { bytes = msg_buffer; len = msg_len }));

          if len - 5 > msg_len then
            read_message ~data ~off:(msg_len + 5)
              ~len:(len - msg_len - 5)
              msg_stream state)
        else
          let msg_buffer = take_buffer msg_len in

          Bigstringaf.blit_to_bytes data ~src_off:(off + 5) msg_buffer
            ~dst_off:0 ~len:(len - 5);

          state := PartialBody (msg_buffer, len - 5, msg_len))
      else
        let header_buffer = Bigstringaf.create 5 in
        Bigstringaf.blit data ~src_off:off header_buffer ~dst_off:0 ~len;
        state := PartialHeader (header_buffer, len)
  | PartialHeader (bs, read_part) ->
      let remaining_header_len = 5 - read_part in
      if len >= remaining_header_len then (
        Bigstringaf.blit data ~src_off:off bs ~dst_off:read_part
          ~len:remaining_header_len;
        let msg_len = extract_msg_len ~data:bs ~off:1 in
        let rest_len = len - remaining_header_len in

        if rest_len >= msg_len then (
          let msg_buffer = take_buffer msg_len in
          Bigstringaf.blit_to_bytes data
            ~src_off:(off + remaining_header_len)
            msg_buffer ~dst_off:0 ~len:msg_len;

          Eio.Stream.add msg_stream
            (Some (to_consumer { bytes = msg_buffer; len = msg_len }));
          if rest_len > msg_len then (
            state := Idle;
            read_message ~data
              ~off:(off + remaining_header_len + msg_len)
              ~len:(len - remaining_header_len - msg_len)
              msg_stream state))
        else
          let msg_buffer = take_buffer msg_len in
          Bigstringaf.blit_to_bytes data
            ~src_off:(off + remaining_header_len)
            msg_buffer ~dst_off:0 ~len:rest_len;

          state := PartialBody (msg_buffer, rest_len, msg_len))
      else (
        Bigstringaf.blit data ~src_off:off bs ~dst_off:read_part ~len;
        state := PartialHeader (bs, read_part + len))
  | PartialBody (body_buffer, read, msg_len) ->
      let remaining_body_len = msg_len - read in

      if len >= remaining_body_len then (
        Bigstringaf.blit_to_bytes data ~src_off:off body_buffer ~dst_off:read
          ~len:remaining_body_len;

        Eio.Stream.add msg_stream
          (Some (to_consumer { bytes = body_buffer; len = msg_len }));
        state := Idle;

        if len > remaining_body_len then
          read_message ~data ~off:(off + remaining_body_len)
            ~len:(len - remaining_body_len) msg_stream state)
      else (
        Bigstringaf.blit_to_bytes data ~src_off:off body_buffer ~dst_off:read
          ~len;
        state := PartialBody (body_buffer, read + len, msg_len))
