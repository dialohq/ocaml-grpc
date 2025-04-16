module type Buffer = sig
  type t

  val create : int -> t
  val len : t -> int
end

module Bigstring_buffer = struct
  type t = Bigstringaf.t

  let create len = Bigstringaf.create len
  let len t = Bigstringaf.length t
end

module Bytes_buffer = struct
  type t = Bytes.t

  let create len = Bytes.create len
  let len t = Bytes.length t
end

module M = Map.Make (struct
  type t = int

  let compare = compare
end)

module MakePool (Buf : Buffer) = struct
  module M = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type buffer = Buf.t
  type t = { mutable available : buffer list M.t }

  let make () = { available = M.empty }

  let pop m i f =
    match M.find_opt i m with
    | Some [] -> raise @@ Invalid_argument "Empty list in map"
    | Some [ b ] -> (M.remove i m, b)
    | Some (b :: bs) ->
        let m' = M.remove i m |> M.add i bs in
        (m', b)
    | None -> (m, f ())

  let push m v =
    let i = Buf.len v in
    match M.find_opt i m with
    | Some bs -> M.add i (v :: bs) m
    | None -> M.add i [ v ] m

  let alloc t size =
    let default () = Buf.create size in
    let i =
      (* Find the lowest possible buffer and check that it isn't too large*)
      match M.find_first_opt (fun i -> i >= size) t.available with
      | Some (i, _) -> if i < 2 * size then i else size
      | None -> size
    in
    let avail', b = pop t.available i default in
    t.available <- avail';
    (* B.reset b; *)
    b

  let release t buf = t.available <- push t.available buf
end

module Bigstring_pool = MakePool (Bigstring_buffer)
module Bytes_pool = MakePool (Bytes_buffer)
