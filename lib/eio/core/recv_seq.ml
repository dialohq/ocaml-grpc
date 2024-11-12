type ('a, 'err) t = unit -> ('a, 'err) recv_item
and ('a, 'err) recv_item = Done | Next of 'a * ('a, 'err) t | Err of 'err

let rec map f recv =
 fun () ->
  match recv () with
  | Done -> Done
  | Next (x, recv) -> Next (f x, map f recv)
  | Err err -> Err err

(* let rec map ~error f recv () = *)
(*   match recv () with *)
(*   | Seq.Nil -> Done *)
(*   | Seq.Cons (x, recv) -> *)
(*       Next *)
(*         ( f x, *)
(*           fun () -> *)
(*             Eio.Fiber.first *)
(*               (fun () -> Err (Eio.Promise.await error)) *)
(*               (fun () -> map ~error f recv ()) ) *)

let to_seq ?err_to_exn recv =
  let rec loop recv () =
    match recv () with
    | Done -> Seq.Nil
    | Next (x, recv) -> Seq.Cons (x, loop recv)
    | Err err -> (
        match err_to_exn with
        | None ->
            failwith
              "Unexpected error on read. Implement err_to_exn for a more \
               granular error."
        | Some f -> raise (f err))
  in
  loop recv
