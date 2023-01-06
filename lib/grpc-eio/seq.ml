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

let peek reader =
  match reader () with Nil -> None | Cons (item, _) -> Some item

let create_reader_writer () =
  let promise, resolver = Promise.create () in
  ((fun () -> Promise.await promise), resolver)
