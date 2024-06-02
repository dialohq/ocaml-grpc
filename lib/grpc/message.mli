val make : string -> string
(** [make s] encodes a string as a gRPC message. *)

val extract : Buffer.t -> string option
(** [extract b] attempts to extract a gRPC message from [b]. *)

val extract_message_pos : start:int -> Bytes.t -> (int * int) option
(** [extract b] attempts to extract a gRPC message from [b] and exposes its
    internal buffer. *)

val extract_all : (string -> unit) -> Buffer.t -> unit
(** [extract_all f b] extracts and calls [f] on all gRPC messages from [b]. *)

type format = [ `Json | `Proto | `Other of string ]

val format_to_content_type : format -> string
(** [format_to_content_type f] returns the content type for [f]. *)
