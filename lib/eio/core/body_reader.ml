open Recv_seq

type t = { bytes : Bytes.t; len : int }

let buffer_count = ref 0
let total_length = ref 0
let m2 = ref 0.0
let mean = ref 0.0
let buckets = [| 0; 0; 0; 0; 0 |]
(* Corresponds to ranges 0-100, 101-300, 301-700, 701-1000, 1001+ *)

let free _ = decr buffer_count

(* Function to update mean and variance dynamically *)
let update_statistics new_len =
  let new_count = float_of_int (!buffer_count + 1) in
  let delta = float_of_int new_len -. !mean in
  mean := !mean +. (delta /. new_count);
  let delta2 = float_of_int new_len -. !mean in
  m2 := !m2 +. (delta *. delta2 (* This is an online formula for variance *));
  let index =
    if new_len <= 100 then 0
    else if new_len <= 300 then 1
    else if new_len <= 700 then 2
    else if new_len <= 1000 then 3
    else 4
  in
  buckets.(index) <- buckets.(index) + 1

let get_next msg_len =
  incr buffer_count;
  update_statistics msg_len;
  total_length := !total_length + msg_len;
  Bytes.create msg_len

(* Calculate average message length *)
let average_msg_len () =
  float_of_int !total_length /. float_of_int !buffer_count

(* Calculate standard deviation of message length *)
let stddev_msg_len () = sqrt (!m2 /. float_of_int !buffer_count)

(* Function to get the counts of each bucket *)
let get_buckets () = buckets

type 'a consumer = { consume : 'b. ('a -> 'b) -> 'b }

let to_consumer t =
  {
    consume =
      (fun f ->
        let res = f t in
        free t;
        res);
  }

let extract_msg_len ~data ~off =
  let high = Bigstringaf.get_int16_be data off in
  let low = Bigstringaf.get_int16_be data (off + 2) in
  (high lsl 16) lor low

let rec unwrap_message_with_header ~data ~off ~len ~into:promise ~read_next
    ~read_more =
  if len >= 5 then
    let _compressed = Bigstringaf.get data off in
    let msg_len = extract_msg_len ~data ~off:(off + 1) in
    unwrap_message ~msg_len ~data ~off:(off + 5) ~len:(len - 5) ~into:promise
      ~read_next ~read_more
  else
    let header_buffer = Bigstringaf.create 5 in
    Bigstringaf.blit data ~src_off:off header_buffer ~dst_off:0 ~len;
    read_more (`Header (header_buffer, 5 - len)) ~into:promise

and unwrap_message ~msg_len ~data ~off ~len ~into:promise ~read_next ~read_more
    =
  if len >= msg_len then (
    let bytes = get_next msg_len in
    let next_decoder = bytes in
    Bigstringaf.blit_to_bytes data ~src_off:off bytes ~dst_off:0 ~len:msg_len;
    if len = msg_len then
      Eio.Promise.resolve promise
        (Next
           ( to_consumer { bytes = next_decoder; len = msg_len },
             fun () -> read_next () ))
    else
      let next, next_u = Eio.Promise.create () in
      unwrap_message_with_header ~data ~off:(off + msg_len) ~len:(len - msg_len)
        ~into:next_u ~read_more ~read_next;

      Eio.Promise.resolve promise
        (Next
           ( to_consumer { bytes = next_decoder; len = msg_len },
             fun () -> Eio.Promise.await next )))
  else
    let bytes = Bytes.create msg_len in
    Bigstringaf.blit_to_bytes data ~src_off:off bytes ~dst_off:0 ~len;
    read_more (`Body (bytes, msg_len, msg_len - len)) ~into:promise

let rec read_more schedule_read buffer ~into:promise =
  schedule_read
    ~on_eof:(fun () -> Eio.Promise.resolve promise (Err `Unexpected_eof))
    ~on_read:(fun bigstring ~off ~len ->
      match buffer with
      | `Header (buffer, remaining) ->
          if len < remaining then (
            Bigstringaf.blit bigstring ~src_off:off buffer
              ~dst_off:(5 - remaining) ~len;
            read_more schedule_read
              (`Header (buffer, remaining - len))
              ~into:promise)
          else (
            Bigstringaf.blit bigstring ~src_off:off buffer
              ~dst_off:(5 - remaining) ~len:remaining;
            let _compressed = Bigstringaf.get buffer off in
            let msg_len = extract_msg_len ~data:buffer ~off:(off + 1) in
            unwrap_message ~msg_len ~data:buffer ~off:remaining
              ~len:(len - remaining) ~into:promise
              ~read_next:(fun () -> read_next schedule_read)
              ~read_more:(read_more schedule_read))
      | `Body (buffer, msg_len, remaining) ->
          if len >= remaining then (
            Bigstringaf.blit_to_bytes bigstring ~src_off:off buffer
              ~dst_off:(msg_len - remaining) ~len:remaining;
            if len > remaining then (
              let next, next_u = Eio.Promise.create () in
              unwrap_message_with_header ~data:bigstring ~off:(off + remaining)
                ~len:(len - remaining) ~into:next_u
                ~read_next:(fun () -> read_next schedule_read)
                ~read_more:(read_more schedule_read);
              Eio.Promise.resolve promise
                (Next
                   ( to_consumer { bytes = buffer; len = msg_len },
                     fun () -> Eio.Promise.await next )))
            else
              Eio.Promise.resolve promise
                (Next
                   ( to_consumer { bytes = buffer; len = msg_len },
                     fun () -> read_next schedule_read )))
          else (
            Bigstringaf.blit_to_bytes bigstring ~src_off:off buffer
              ~dst_off:(msg_len - remaining) ~len;
            read_more schedule_read
              (`Body (buffer, msg_len, remaining - len))
              ~into:promise))

and read_next schedule_read =
  let promise, promise_u = Eio.Promise.create () in
  schedule_read
    ~on_eof:(fun () -> Eio.Promise.resolve promise_u Done)
    ~on_read:(fun bigstring ~off ~len ->
      unwrap_message_with_header ~data:bigstring ~off ~len ~into:promise_u
        ~read_next:(fun () -> read_next schedule_read)
        ~read_more:(read_more schedule_read));
  Eio.Promise.await promise

let fill_header ~pos ~length buffer =
  (* write compressed flag (uint8) *)
  Bigstringaf.set buffer pos '\x00';
  (* write msg length (uint32 be) *)
  Bigstringaf.set_int16_be buffer (pos + 1) (length lsr 16);
  Bigstringaf.set_int16_be buffer (pos + 3) (length land 0xFFFF)

exception Unexpected_eof

let to_seq_exn =
  let rec iter s =
    match s () with
    | Next (msg, cons) -> Seq.Cons (msg, fun () -> iter cons)
    | Done -> Seq.Nil
    | Err `Unexpected_eof -> raise Unexpected_eof
  in
  fun sequence () -> iter sequence

let%expect_test "extracting multiple messages" =
  Eio_mock.Backend.run @@ fun _env ->
  let promise, promise_u = Eio.Promise.create () in
  let test_buffer = Bigstringaf.create ((3 * 5) + 1 + 2 + 3) in
  fill_header ~pos:0 ~length:1 test_buffer;
  fill_header ~pos:6 ~length:2 test_buffer;
  fill_header ~pos:13 ~length:3 test_buffer;
  Bigstringaf.blit_from_string "1" ~src_off:0 test_buffer ~dst_off:5 ~len:1;
  Bigstringaf.blit_from_string "22" ~src_off:0 test_buffer ~dst_off:11 ~len:2;
  Bigstringaf.blit_from_string "333" ~src_off:0 test_buffer ~dst_off:18 ~len:3;

  unwrap_message_with_header ~data:test_buffer ~off:0
    ~len:(Bigstringaf.length test_buffer)
    ~into:promise_u
    ~read_next:(fun () -> Done)
    ~read_more:(fun _ -> raise Not_found);

  (fun () -> Eio.Promise.await promise)
  |> to_seq_exn
  |> Seq.iter (fun { consume } ->
         consume (fun { bytes; len } ->
             Bytes.sub_string bytes 0 len |> print_endline));
  [%expect {| 
  1
  22
  333
  |}]

let%expect_test "extracting single message" =
  Eio_mock.Backend.run @@ fun _env ->
  let promise, promise_u = Eio.Promise.create () in
  let test_buffer = Bigstringaf.create 6 in
  fill_header ~pos:0 ~length:1 test_buffer;
  Bigstringaf.blit_from_string "1" ~src_off:0 test_buffer ~dst_off:5 ~len:1;

  unwrap_message_with_header ~data:test_buffer ~off:0
    ~len:(Bigstringaf.length test_buffer)
    ~into:promise_u
    ~read_next:(fun () -> Done)
    ~read_more:(fun _ -> raise Not_found);

  (fun () -> Eio.Promise.await promise)
  |> to_seq_exn
  |> Seq.iter (fun { consume } ->
         consume (fun { bytes; len } ->
             Bytes.sub_string bytes 0 len |> print_endline));
  [%expect {| 
  1
  |}]

let%test_module "reading body" =
  (module struct
    let get_reader reads =
      let buffer = Bigstringaf.create 65536 in
      let packets = ref reads in
      fun ~on_eof ~on_read ->
        match !packets with
        | [] -> on_eof ()
        | packet :: rest ->
            packets := rest;
            on_read buffer ~off:0 ~len:(packet buffer)

    let%test "reading partial body (error)" =
      let schedule_read =
        get_reader
          [
            (fun buf ->
              fill_header ~pos:0 ~length:3 buf;
              5);
            (fun buf ->
              Bigstringaf.blit_from_string "12" ~src_off:0 buf ~dst_off:0 ~len:2;
              2);
          ]
      in
      let result = read_next schedule_read in
      match result with Err `Unexpected_eof -> true | _ -> false

    let%expect_test "reading body in multiple chunks" =
      Eio_mock.Backend.run @@ fun _env ->
      let header = Bigstringaf.create 5 in
      fill_header ~pos:0 ~length:10 header;
      let schedule_read =
        get_reader
          [
            (fun buf ->
              Bigstringaf.blit header ~src_off:0 buf ~dst_off:0 ~len:3;
              3);
            (fun buf ->
              Bigstringaf.blit header ~src_off:3 buf ~dst_off:0 ~len:2;
              2);
            (fun buf ->
              Bigstringaf.blit_from_string "55555" ~src_off:0 buf ~dst_off:0
                ~len:5;
              5);
            (fun buf ->
              Bigstringaf.blit_from_string "55555" ~src_off:0 buf ~dst_off:0
                ~len:5;
              5);
          ]
      in
      let result = read_next schedule_read in
      (match result with
      | Done -> print_endline "failure"
      | Err `Unexpected_eof -> print_endline "failure"
      | Next ({ consume }, cons) -> (
          print_endline
            (consume (fun { bytes; len } -> Bytes.sub_string bytes 0 len));
          match cons () with
          | Done -> ()
          | Err `Unexpected_eof | Next _ -> failwith "expected end of sequence"));
      [%expect "5555555555"]
  end)
