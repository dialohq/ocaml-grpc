include module type of Stdlib.Seq

type 'a reader = 'a t
type 'a writer

val create_reader_writer : unit -> 'a reader * 'a writer
val read : 'a reader -> 'a Stdlib.Seq.node
val read_and_exhaust : 'a reader -> 'a option
val exhaust_reader : 'a reader -> unit
val write : 'a writer -> 'a -> unit
val close_writer : 'a writer -> unit
(* val map_writer : ('a -> 'b) -> 'a writer -> 'b writer *)
