type t = string option Eio.Stream.t

let make
    ~(schedule_read_raw :
       on_eof:(unit -> unit) ->
       on_read:(Bigstringaf.t -> off:int -> len:int -> unit) ->
       unit) =
  let buffer = Grpc.Buffer.v () in
  let stream = Eio.Stream.create max_int in
  let on_msg msg = Eio.Stream.add stream (Some msg) in
  let on_eof () = Eio.Stream.add stream None in
  let rec on_read src ~off ~len =
    Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src ~dst:buffer ~length:len;
    Grpc.Message.extract_all on_msg buffer;
    schedule_read_raw ~on_read ~on_eof
  in
  schedule_read_raw ~on_read ~on_eof;
  stream

let to_seq t =
  let rec seq () =
    match Eio.Stream.take t with
    | Some msg -> Seq.Cons (msg, seq)
    | None -> Seq.Nil
  in
  seq
