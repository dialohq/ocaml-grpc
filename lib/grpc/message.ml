let make content =
  let content_len = String.length content in
  let payload = Bytes.create @@ (content_len + 1 + 4) in
  (* write compressed flag (uint8) *)
  Bytes.set payload 0 '\x00';
  (* write msg length (uint32 be) *)
  let length = String.length content in
  Bytes.set_uint16_be payload 1 (length lsr 16);
  Bytes.set_uint16_be payload 3 (length land 0xFFFF);
  (* write msg *)
  Bytes.blit_string content 0 payload 5 content_len;
  Bytes.to_string payload

(** [extract_message buf] extracts the grpc message starting in [buf]
    in the buffer if there is one *)
let extract_message buf =
  if Buffer.length buf >= 5 then (
    let compressed =
      (* A Compressed-Flag value of 1 indicates that the binary octet
         sequence of Message is compressed using the mechanism declared by
         the Message-Encoding header. A value of 0 indicates that no encoding
         of Message bytes has occurred. Compression contexts are NOT
         maintained over message boundaries, implementations must create a
         new context for each message in the stream. If the Message-Encoding
         header is omitted then the Compressed-Flag must be 0. *)
      (* encoded as 1 byte unsigned integer *)
      Buffer.get_u8 buf ~pos:0 == 1
    and length =
      (* encoded as 4 byte unsigned integer (big endian) *)
      Buffer.get_u32_be buf ~pos:1
    in
    if compressed then failwith "Compressed flag set but not supported";
    if Buffer.length buf - 5 >= length then
      Some (Buffer.sub buf ~start:5 ~length |> Buffer.to_string)
    else None)
  else None

(** [get_message_and_shift buf] tries to extract the first grpc message
    from [buf] and if successful shifts these bytes out of the buffer *)
let get_message_and_shift buf =
  let message = extract_message buf in
  match message with
  | None -> None
  | Some message ->
      let mlen = String.length message in
      Buffer.shift_left buf ~by:(5 + mlen);
      Some message

let extract buf = get_message_and_shift buf
