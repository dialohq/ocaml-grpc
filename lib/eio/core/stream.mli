type t = string option Eio.Stream.t

(* Stream of string-encoded grpc messages *)
val make :
  schedule_read_raw:
    (on_eof:(unit -> unit) ->
    on_read:(Bigstringaf.t -> off:int -> len:int -> unit) ->
    unit) ->
  string option Eio.Stream.t

val to_seq : string option Eio.Stream.t -> string Seq.t
