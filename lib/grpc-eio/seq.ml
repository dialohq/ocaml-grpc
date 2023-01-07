include Stdlib.Seq
open Eio

type 'a reader = 'a t
type 'a writer = 'a node Promise.u

let write writer item =
  let promise, resolver = Promise.create () in
  let next = Cons (item, fun () -> Promise.await promise) in
  Promise.resolve writer next;
  resolver

let close_writer writer = Promise.resolve writer Nil
let read reader = reader ()

let rec exhaust_reader reader =
  match reader () with Nil -> () | Cons (_, reader) -> exhaust_reader reader

let read_and_exhaust reader =
  match reader () with
  | Nil -> None
  | Cons (item, reader) ->
      exhaust_reader reader;
      Some item

let create_reader_writer () =
  let promise, resolver = Promise.create () in
  ((fun () -> Promise.await promise), resolver)
