type 'a t = 'a Eio.Stream.t

type 'a reader = {
  len : 'a -> int;
  grow : 'a -> int -> unit;
}

let make ~decode
    ~(schedule_read_raw :
       on_eof:(unit -> unit) ->
       on_read:(Bigstringaf.t -> off:int -> len:int -> unit) ->
       unit) =
  let stream = Eio.Stream.create max_int in
  let on_msg msg = Eio.Stream.add stream (Some msg) in
  let on_eof () = Eio.Stream.add stream None in

  let rec on_read src ~off ~len =
    decode src ~off ~len ~push_msg
    schedule_read_raw ~on_read ~on_eof
  in
  schedule_read_raw ~on_read ~on_eof;
  stream
