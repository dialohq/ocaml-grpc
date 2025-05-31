type single_writer = Pbrt.Encoder.t -> unit
type 'a stream_writer = 'a -> (Pbrt.Encoder.t -> unit) option * 'a
type 'a stream_reader = 'a -> Pbrt.Decoder.t option -> 'a
type format = [ `Json | `Proto | `Other of string ]

let fill_header ~length (buffer : Cstruct.t) =
  Cstruct.set_char buffer 0 '\x00';
  Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
  Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)
