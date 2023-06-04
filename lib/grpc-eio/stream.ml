type t = { body : H2.Body.Reader.t; buffer : Grpc.Buffer.t }

let of_h2_body body =
  let buffer = Grpc.Buffer.v () in
  { body; buffer }

let schedule_read ~on_msg ~on_eof { body; buffer } =
  let rec on_read src ~off ~len =
    Grpc.Buffer.copy_from_bigstringaf ~src_off:off ~src ~dst:buffer ~length:len;
  Grpc.Message.extract_all on_msg request_buffer;
  H2.Body.Reader.schedule_read body ~on_read ~on_eof

let take stream =
  let promise, resolver = Eio.Promise.create () in
  let on_msg msg = Eio.Promise.resolve resolver (Some msg) in
  let on_eof () = Eio.Promise.resolve resolver None in
  schedule_read ~on_msg ~on_eof stream;
  promise

let to_seq stream =
  let reader, writer = Seq.create_reader_writer () in
  let on_eof () = Seq.close_writer writer in
  let rec read_loop () =
    let on_msg msg =
      Seq.write writer msg;
      read_loop ()
    in
    schedule_read ~on_msg ~on_eof stream
  in
  read_loop ();
  reader
