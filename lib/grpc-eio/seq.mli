include module type of Stdlib.Seq

type 'a reader = 'a t
type 'a writer

val create_reader_writer : unit -> 'a reader * 'a writer
val read : 'a reader -> 'a Stdlib.Seq.node
val peek : 'a reader -> 'a option
val write : 'a writer -> 'a -> 'a writer
val close_writer : 'a writer -> unit