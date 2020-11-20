val make : string -> string
(** [make s] encodes a string as a gRPC message. *)

val extract : Buffer.t -> Buffer.t option
(** [extract b] attempts to extract a gRPC message from [b]. *)
