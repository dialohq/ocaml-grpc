type t

val of_h2_body : H2.Body.Reader.t -> t

val schedule_read :
  on_msg:(string -> unit) -> on_eof:(unit -> unit) -> t -> unit

val read_loop : on_msg:(string -> unit) -> on_eof:(unit -> unit) -> t -> unit
val take : t -> string option Eio.Promise.t
val to_seq : t -> string Seq.t
