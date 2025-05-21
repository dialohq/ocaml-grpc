module Grpc_utils = struct
  module Buffer = struct
    type t = { mutable contents : bytes; mutable length : int }

    let v ?(capacity = 1024) () =
      { contents = Bytes.create capacity; length = 0 }

    let length t = t.length
    let capacity t = Bytes.length t.contents
    let extend t amount = t.contents <- Bytes.extend t.contents 0 amount

    let rec nearest_power_of_2 acc target =
      if acc >= target then acc else nearest_power_of_2 (acc * 2) target

    let ensure_size t ~extra =
      let current_capacity = capacity t in
      let needed_capacity = t.length + extra in
      if needed_capacity >= current_capacity then
        extend t
          (nearest_power_of_2 current_capacity needed_capacity
          - current_capacity)

    let copy_from_bigstringaf ~src_off ~src ~dst ~length =
      ensure_size dst ~extra:length;
      Bigstringaf.blit_to_bytes ~src_off src ~dst_off:dst.length dst.contents
        ~len:length;
      dst.length <- dst.length + length

    let sub_string ~start ~length t = Bytes.sub_string t.contents start length

    let shift_left ~by t =
      Bytes.blit t.contents by t.contents 0 (t.length - by);
      t.length <- t.length - by

    let get_u8 ~pos t = Bytes.get_uint8 t.contents pos

    let get_u32_be ~pos t =
      let high = Bytes.get_uint16_be t.contents pos in
      let low = Bytes.get_uint16_be t.contents (pos + 2) in
      (high lsl 16) lor low

    let internal_buffer t = t.contents
  end

  module Message = struct
    [@@@landmark "auto"]

    let fill_header ~length buffer =
      (* write compressed flag (uint8) *)
      Bytes.set buffer 0 '\x00';
      (* write msg length (uint32 be) *)
      Bytes.set_uint16_be buffer 1 (length lsr 16);
      Bytes.set_uint16_be buffer 3 (length land 0xFFFF)

    let make content =
      let content_len = String.length content in
      let payload = Bytes.create @@ (content_len + 1 + 4) in
      fill_header ~length:content_len payload;
      (* write msg *)
      Bytes.blit_string content 0 payload 5 content_len;
      Bytes.to_string payload

    let get_u32_be ~pos t =
      let high = Bytes.get_uint16_be t pos in
      let low = Bytes.get_uint16_be t (pos + 2) in
      (high lsl 16) lor low

    (** [extract_message buf] extracts the grpc message starting in [buf] in the
        buffer if there is one *)
    let extract_message_pos ~start buf =
      if Bytes.length buf >= 5 + start then (
        let compressed =
          (* A Compressed-Flag value of 1 indicates that the binary octet
         sequence of Message is compressed using the mechanism declared by
         the Message-Encoding header. A value of 0 indicates that no encoding
         of Message bytes has occurred. Compression contexts are NOT
         maintained over message boundaries, implementations must create a
         new context for each message in the stream. If the Message-Encoding
         header is omitted then the Compressed-Flag must be 0. *)
          (* encoded as 1 byte unsigned integer *)
          Bytes.get_uint8 buf start == 1
        and length =
          (* encoded as 4 byte unsigned integer (big endian) *)
          get_u32_be buf ~pos:(start + 1)
        in
        if compressed then failwith "Compressed flag set but not supported";
        if Bytes.length buf - 5 >= length then Some (start + 5, length)
        else None)
      else None

    (** [get_message_and_shift buf] tries to extract the first grpc message from
        [buf] and if successful shifts these bytes out of the buffer *)
    let get_message_and_shift buf =
      match extract_message_pos ~start:0 (Buffer.internal_buffer buf) with
      | None -> None
      | Some (start, length) ->
          let message = Buffer.sub_string ~start ~length buf in
          let mlen = String.length message in
          Buffer.shift_left buf ~by:(5 + mlen);
          Some message

    let extract buf = get_message_and_shift buf

    let extract_all f buf =
      let rec loop () =
        match extract buf with
        | None -> ()
        | Some message ->
            f message;
            loop ()
      in
      loop ()
  end

  module Status = struct
    type code =
      | OK
      | Cancelled
      | Unknown
      | Invalid_argument
      | Deadline_exceeded
      | Not_found
      | Already_exists
      | Permission_denied
      | Resource_exhausted
      | Failed_precondition
      | Aborted
      | Out_of_range
      | Unimplemented
      | Internal
      | Unavailable
      | Data_loss
      | Unauthenticated
    [@@deriving show]

    let int_of_code = function
      | OK -> 0
      | Cancelled -> 1
      | Unknown -> 2
      | Invalid_argument -> 3
      | Deadline_exceeded -> 4
      | Not_found -> 5
      | Already_exists -> 6
      | Permission_denied -> 7
      | Resource_exhausted -> 8
      | Failed_precondition -> 9
      | Aborted -> 10
      | Out_of_range -> 11
      | Unimplemented -> 12
      | Internal -> 13
      | Unavailable -> 14
      | Data_loss -> 15
      | Unauthenticated -> 16

    let code_of_int = function
      | 0 -> Some OK
      | 1 -> Some Cancelled
      | 2 -> Some Unknown
      | 3 -> Some Invalid_argument
      | 4 -> Some Deadline_exceeded
      | 5 -> Some Not_found
      | 6 -> Some Already_exists
      | 7 -> Some Permission_denied
      | 8 -> Some Resource_exhausted
      | 9 -> Some Failed_precondition
      | 10 -> Some Aborted
      | 11 -> Some Out_of_range
      | 12 -> Some Unimplemented
      | 13 -> Some Internal
      | 14 -> Some Unavailable
      | 15 -> Some Data_loss
      | 16 -> Some Unauthenticated
      | _ -> None

    type t = { code : code; message : string option } [@@deriving show]

    let make ?error_message code = { code; message = error_message }
    let code t = t.code

    let error_message t =
      Option.map (fun message -> Uri.pct_encode message) t.message

    let extract_status ~get_header =
      let code, message =
        match get_header "grpc-status" with
        | None -> (Unknown, Some "Expected gprc-status header, got nothing")
        | Some s -> (
            match int_of_string_opt s with
            | None ->
                let msg =
                  Printf.sprintf "Expected valid gprc-status header, got %s" s
                in
                (Unknown, Some msg)
            | Some i -> (
                match code_of_int i with
                | None ->
                    let msg =
                      Printf.sprintf "Expected valid gprc-status code, got %i" i
                    in
                    (Unknown, Some msg)
                | Some c -> (c, get_header "grpc-message")))
      in
      make ?error_message:message code

    let status_to_headers status =
      let message = error_message status in
      ("grpc-status", string_of_int (int_of_code (code status)))
      :: (match message with Some s -> [ ("grpc-message", s) ] | None -> [])

    let to_net_resp status =
      (* https://cloud.google.com/apis/design/errors#error_model *)
      let headers = status_to_headers status in
      let status_code =
        match code status with
        | OK -> 200
        | Cancelled -> 499
        | Unknown -> 500
        | Invalid_argument -> 400
        | Deadline_exceeded -> 504
        | Not_found -> 404
        | Already_exists -> 409
        | Permission_denied -> 403
        | Resource_exhausted -> 429
        | Failed_precondition -> 400
        | Aborted -> 409
        | Out_of_range -> 400
        | Unimplemented -> 501
        | Internal -> 500
        | Unavailable -> 503
        | Data_loss -> 500
        | Unauthenticated -> 401
      in
      (status_code, headers)
  end
end

module Grpc_client = struct
  module Grpc = Grpc_utils

  let status_of_trailers ~get_header =
    match get_header "grpc-status" with
    | None ->
        Grpc.Status.make ~error_message:"Server did not return grpc-status"
          Grpc.Status.Unknown
    | Some s -> (
        match Option.bind (int_of_string_opt s) Grpc.Status.code_of_int with
        | None ->
            Grpc.Status.make
              ~error_message:
                (Printf.sprintf "Server returned an invalid grpc-status %s" s)
              Grpc.Status.Unknown
        | Some status ->
            Grpc.Status.make ?error_message:(get_header "grpc-message") status)

  let trailers_missing_status =
    Grpc.Status.make ~error_message:"Trailers missing" Grpc.Status.Unknown
end

module Grpc_server = struct
  module StringMap = Map.Make (String)

  type error =
    [ `Not_found of [ `Service_not_found | `Invalid_url | `Bad_method ]
    | `Unsupported_media_type
    | `Bad_request
    | `Grpc of Grpc_utils.Status.t ]

  let error_to_code_and_headers error =
    match error with
    | `Not_found _ -> (404, [])
    | `Unsupported_media_type -> (415, [])
    | `Bad_request -> (400, [])
    | `Grpc status -> Grpc_utils.Status.to_net_resp status

  let rec service_name_and_method = function
    | [] -> None
    | [ _ ] -> None
    | [ service_name; method_name ] -> Some (service_name, method_name)
    | _ :: tl -> service_name_and_method tl

  type parsed_request = { service : string; meth : string }

  let parse_request ~is_post_request ~get_header ~path :
      (parsed_request, error) result =
    let route () =
      let parts = String.split_on_char '/' path in
      match service_name_and_method parts with
      | Some (service, meth) -> Ok { service; meth }
      | None -> Error (`Not_found `Invalid_url)
    in
    match is_post_request with
    | true -> (
        match get_header "content-type" with
        | Some s ->
            if
              Stringext.chop_prefix s ~prefix:"application/grpc"
              |> Option.is_some
            then
              match get_header "grpc-encoding" with
              | None | Some "identity" -> (
                  match get_header "grpc-accept-encoding" with
                  | None -> route ()
                  | Some encodings ->
                      let encodings = String.split_on_char ',' encodings in
                      if List.mem "identity" encodings then route ()
                      else Error (`Grpc (Grpc_utils.Status.make Unimplemented)))
              | Some _ ->
                  (* TODO: not sure if there is a specific way to handle this in grpc *)
                  Error `Bad_request
            else Error `Unsupported_media_type
        | None -> Error `Unsupported_media_type)
    | _ -> Error (`Not_found `Bad_method)

  type headers = { content_type : string; extra : (string * string) list }
  type format = [ `Json | `Proto | `Other of string ]

  let headers ?(extra = []) (format : format) =
    {
      content_type =
        (match format with
        | `Json -> "application/grpc+json"
        | `Proto -> "application/grpc+proto"
        | `Other s -> Printf.sprintf "application/grpc+%s" s);
      extra;
    }

  let headers_grpc_proto = headers `Proto

  type trailers = {
    grpc_status : int;
    grpc_message : string option;
    extra : (string * string) list;
  }

  let make_trailers ?(extra = []) status =
    {
      grpc_status =
        Grpc_utils.Status.int_of_code (Grpc_utils.Status.code status);
      grpc_message = Grpc_utils.Status.error_message status;
      extra;
    }
end

module Io = struct
  type 'request streaming_writer = {
    (* replace dis string *)
    write : 'request -> unit;
    close : unit -> unit;
    write_trailers : Grpc_server.trailers -> unit;
    is_closed : unit -> bool;
  }

  module type S = sig
    type request

    module Net_request : sig
      type t

      val body : t -> request Seq.t
      val is_post : t -> bool
      val target : t -> string
      val get_header : t -> string -> string option
    end

    type response

    val respond_streaming :
      headers:Grpc_server.headers -> Net_request.t -> response streaming_writer

    val respond_error :
      status_code:int -> headers:(string * string) list -> Net_request.t -> unit
  end

  type ('net_request, 'request, 'response) t =
    (module S
       with type Net_request.t = 'net_request
        and type request = 'request
        and type response = 'response)
end

module Grpc_server_eio = struct
  module Grpc = Grpc_utils
  module Io = Io

  type extra_trailers = (string * string) list

  exception Server_error of Grpc.Status.t * (string * string) list

  module Rpc = struct
    type ('request, 'response) unary =
      'request -> 'response * (string * string) list

    type ('req, 'res) client_streaming =
      'req Seq.t -> 'res * (string * string) list

    type ('req, 'res) server_streaming =
      'req -> ('res -> unit) -> (string * string) list

    type ('req, 'res) bidirectional_streaming =
      'req Seq.t -> ('res -> unit) -> (string * string) list

    type ('req, 'res) rpc_impl =
      'req Seq.t -> ('res -> unit) -> (string * string) list

    type rpc_complete = Rpc_complete

    type ('req, 'res) handler_accept = {
      accept :
        Grpc_server.headers ->
        ('req Seq.t -> ('res -> unit) -> extra_trailers) ->
        rpc_complete;
    }

    type ('net_req, 'req, 'res) handler =
      'net_req -> ('req, 'res) handler_accept -> rpc_complete

    let unary (unary_handler : _ unary) : _ rpc_impl =
     fun request_stream respond ->
      match request_stream () with
      | Seq.Cons (request, _) ->
          let response, extra = unary_handler request in
          respond response;
          extra
          (* TODO: Look up which error this is *)
      | Seq.Nil -> raise (Server_error (Grpc.Status.make Not_found, []))

    let client_streaming (client_streaming_handler : _ client_streaming) :
        _ rpc_impl =
     fun request_stream respond ->
      let response, extra = client_streaming_handler request_stream in
      respond response;
      extra

    let server_streaming (server_streaming_handler : _ server_streaming) :
        _ rpc_impl =
     fun requests respond ->
      match requests () with
      | Seq.Cons (request, _) -> server_streaming_handler request respond
      | Seq.Nil -> raise (Server_error (Grpc.Status.make Not_found, []))
    (* TODO: Look up which error this is *)
  end

  module G = Grpc_server

  type ('net_request, 'req, 'resp) t =
    service:string -> meth:string -> ('net_request, 'req, 'resp) Rpc.handler

  let handle_request ?error_handler (type net_request req resp)
      ~(io : (net_request, req, resp) Io.t) server request =
    let module Io' = (val io) in
    let run_handler handler =
      let Rpc.Rpc_complete =
        handler request
          {
            Rpc.accept =
              (fun headers f ->
                let { Io.write; write_trailers; close; is_closed } =
                  Io'.respond_streaming ~headers request
                in
                try
                  let request_stream = Io'.Net_request.body request in
                  let extra = f request_stream write in
                  write_trailers
                    (Grpc_server.make_trailers ~extra (Grpc.Status.make OK));
                  close ();
                  Rpc_complete
                with
                | Server_error (status, extra) ->
                    if not (is_closed ()) then (
                      write_trailers (Grpc_server.make_trailers ~extra status);
                      close ());
                    Rpc_complete
                | exn ->
                    let extra =
                      Option.map (fun f -> f exn) error_handler
                      |> Option.value ~default:[]
                    in
                    if not (is_closed ()) then (
                      write_trailers
                        (Grpc_server.make_trailers ~extra
                           (Grpc.Status.make Internal));
                      close ());
                    Rpc_complete);
          }
      in
      ()
    in
    match
      G.parse_request
        ~is_post_request:(Io'.Net_request.is_post request)
        ~get_header:(fun header -> Io'.Net_request.get_header request header)
        ~path:(Io'.Net_request.target request)
      |> Result.map (fun { G.service; meth } -> server ~service ~meth)
      |> Result.map run_handler
    with
    | Ok () -> ()
    | exception Server_error (status, extra) ->
        let status_code, headers' = Grpc.Status.to_net_resp status in
        let headers = List.concat [ headers'; extra ] in
        Io'.respond_error request ~status_code ~headers
    | exception exn ->
        let headers =
          Option.map (fun f -> f exn) error_handler |> Option.value ~default:[]
        in
        Io'.respond_error request ~status_code:500 ~headers
    | Error e ->
        let status_code, headers = Grpc_server.error_to_code_and_headers e in
        Io'.respond_error request ~status_code ~headers
end

module Recv_seq = struct
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
end

module Body_reader = struct
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

  and unwrap_message ~msg_len ~data ~off ~len ~into:promise ~read_next
      ~read_more =
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
        unwrap_message_with_header ~data ~off:(off + msg_len)
          ~len:(len - msg_len) ~into:next_u ~read_more ~read_next;

        Eio.Promise.resolve promise
          (Next
             ( to_consumer { bytes = next_decoder; len = msg_len },
               fun () -> Eio.Promise.await next )))
    else
      let bytes = Bytes.create msg_len in
      Bigstringaf.blit_to_bytes data ~src_off:off bytes ~dst_off:0 ~len;
      read_more (`Body (bytes, msg_len, msg_len - len)) ~into:promise

  let rec read_more ~error schedule_read buffer ~into:promise =
    schedule_read
      ~on_eof:(fun () -> Eio.Promise.resolve promise (Err `Unexpected_eof))
      ~on_read:(fun bigstring ~off ~len ->
        match buffer with
        | `Header (buffer, remaining) ->
            if len < remaining then (
              Bigstringaf.blit bigstring ~src_off:off buffer
                ~dst_off:(5 - remaining) ~len;
              read_more ~error schedule_read
                (`Header (buffer, remaining - len))
                ~into:promise)
            else (
              Bigstringaf.blit bigstring ~src_off:off buffer
                ~dst_off:(5 - remaining) ~len:remaining;
              let _compressed = Bigstringaf.get buffer off in
              let msg_len = extract_msg_len ~data:buffer ~off:(off + 1) in
              unwrap_message ~msg_len ~data:buffer ~off:remaining
                ~len:(len - remaining) ~into:promise
                ~read_next:(fun () -> read_next ~error schedule_read)
                ~read_more:(read_more ~error schedule_read))
        | `Body (buffer, msg_len, remaining) ->
            if len >= remaining then (
              Bigstringaf.blit_to_bytes bigstring ~src_off:off buffer
                ~dst_off:(msg_len - remaining) ~len:remaining;
              if len > remaining then (
                let next, next_u = Eio.Promise.create () in
                unwrap_message_with_header ~data:bigstring
                  ~off:(off + remaining) ~len:(len - remaining) ~into:next_u
                  ~read_next:(fun () -> read_next ~error schedule_read)
                  ~read_more:(read_more ~error schedule_read);
                Eio.Promise.resolve promise
                  (Next
                     ( to_consumer { bytes = buffer; len = msg_len },
                       fun () -> Eio.Promise.await next )))
              else
                Eio.Promise.resolve promise
                  (Next
                     ( to_consumer { bytes = buffer; len = msg_len },
                       fun () -> read_next ~error schedule_read )))
            else (
              Bigstringaf.blit_to_bytes bigstring ~src_off:off buffer
                ~dst_off:(msg_len - remaining) ~len;
              read_more ~error schedule_read
                (`Body (buffer, msg_len, remaining - len))
                ~into:promise))

  and read_next ~error schedule_read =
    let promise, promise_u = Eio.Promise.create () in
    schedule_read
      ~on_eof:(fun () -> Eio.Promise.resolve promise_u Done)
      ~on_read:(fun bigstring ~off ~len ->
        unwrap_message_with_header ~data:bigstring ~off ~len ~into:promise_u
          ~read_next:(fun () -> read_next ~error schedule_read)
          ~read_more:(read_more ~error schedule_read));
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
                Bigstringaf.blit_from_string "12" ~src_off:0 buf ~dst_off:0
                  ~len:2;
                2);
            ]
        in
        let error, _ = Eio.Promise.create () in
        let result = read_next ~error schedule_read in
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
        let error, _ = Eio.Promise.create () in
        let result = read_next ~error schedule_read in
        (match result with
        | Done -> print_endline "failure"
        | Err _ -> print_endline "failure"
        | Next ({ consume }, cons) -> (
            print_endline
              (consume (fun { bytes; len } -> Bytes.sub_string bytes 0 len));
            match cons () with
            | Done -> ()
            | Err _ | Next _ -> failwith "expected end of sequence"));
        [%expect "5555555555"]
    end)
end
