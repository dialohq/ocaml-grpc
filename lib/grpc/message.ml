[@@@landmark "auto"]

let fill_header ~length buffer =
  (* write compressed flag (uint8) *)
  Bytes.set buffer 0 '\x00';
  (* write msg length (uint32 be) *)
  Bytes.set_uint16_be buffer 1 (length lsr 16);
  Bytes.set_uint16_be buffer 3 (length land 0xFFFF)

let make content =
  let content_len = String.length content in
  let payload = Bytes.create @@ (content_len + 1 + 4) in
  fill_header ~length:content_len payload;
  (* write msg *)
  Bytes.blit_string content 0 payload 5 content_len;
  Bytes.to_string payload

let get_u32_be ~pos t =
  let high = Bytes.get_uint16_be t pos in
  let low = Bytes.get_uint16_be t (pos + 2) in
  (high lsl 16) lor low

(** [extract_message buf] extracts the grpc message starting in [buf] in the
    buffer if there is one *)
let extract_message_pos ~start buf =
  if Bytes.length buf >= 5 + start then (
    let compressed =
      (* A Compressed-Flag value of 1 indicates that the binary octet
         sequence of Message is compressed using the mechanism declared by
         the Message-Encoding header. A value of 0 indicates that no encoding
         of Message bytes has occurred. Compression contexts are NOT
         maintained over message boundaries, implementations must create a
         new context for each message in the stream. If the Message-Encoding
         header is omitted then the Compressed-Flag must be 0. *)
      (* encoded as 1 byte unsigned integer *)
      Bytes.get_uint8 buf start == 1
    and length =
      (* encoded as 4 byte unsigned integer (big endian) *)
      get_u32_be buf ~pos:(start + 1)
    in
    if compressed then failwith "Compressed flag set but not supported";
    if Bytes.length buf - 5 >= length then Some (start + 5, length) else None)
  else None

(** [get_message_and_shift buf] tries to extract the first grpc message from
    [buf] and if successful shifts these bytes out of the buffer *)
let get_message_and_shift buf =
  match extract_message_pos ~start:0 (Buffer.internal_buffer buf) with
  | None -> None
  | Some (start, length) ->
      let message = Buffer.sub_string ~start ~length buf in
      let mlen = String.length message in
      Buffer.shift_left buf ~by:(5 + mlen);
      Some message

let extract buf = get_message_and_shift buf

let extract_all f buf =
  let rec loop () =
    match extract buf with
    | None -> ()
    | Some message ->
        f message;
        loop ()
  in
  loop ()

type format = [ `Json | `Proto | `Other of string ]

let format_to_content_type = function
  | `Json -> "application/grpc+json"
  | `Proto -> "application/grpc+proto"
  | `Other s -> Printf.sprintf "application/grpc+%s" s
