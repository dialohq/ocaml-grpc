open Haha
open Eio

(* (* WARN: remove later *) *)
(* [@@@warning "-27-26"] *)

let status_of_h2_error : Error_code.t -> Status_new.code = function
  (* NO_ERROR case of early shutdown *)
  | NoError | StreamClosed | Cancel -> Cancelled
  | ProtocolError | InternalError | FrameSizeError | CompressionError
  | ConnectError | InadequateSecurity | HTTP_1_1_Required ->
      Internal
  | FlowControlError | EnhanceYourCalm -> Resource_exhausted
  | SettingsTimeout | RefusedStream | UnknownError_code _ -> Unavailable

let status_of_h2_status : Status.t -> Status_new.code = function
  | `OK -> OK
  | `Bad_request -> Invalid_argument
  | `Unauthorized -> Unauthenticated
  | `Forbidden -> Permission_denied
  | `Not_found -> Not_found
  | `Request_timeout -> Deadline_exceeded
  | `Conflict -> Aborted
  | `Too_many_requests -> Resource_exhausted
  | `Bad_gateway | `Service_unavailable | `Gateway_timeout -> Unavailable
  | _ -> Internal

type data_writer = unit -> Cstruct.t list option
type data_receiver = Cstruct.t option -> unit

type stream_request = {
  headers : Grpc_client.request_headers;
  data_writer : data_writer;
  data_receiver : data_receiver;
  path : string;
}

type stream_runner = stream_request -> (unit, Status_new.t) result Promise.t

type connection = {
  start_stream : stream_runner;
  mutable open_streams : int;
  shutdown : unit -> unit;
  error_promise : Status_new.t Promise.t;
}

type connection_pool = connection list ref

type t = {
  start_request : stream_runner;
  connection_pool : connection_pool;
  shutdown : unit -> unit;
}

let create :
    ?streams_per_connection:int -> sw:Switch.t -> net:_ Net.t -> string -> t =
 fun ?(streams_per_connection = 10) ~sw ~net uri ->
  let uri = Uri.of_string uri in
  let host =
    match Uri.host uri with
    | None -> invalid_arg "No host in uri"
    | Some host -> host
  in
  let scheme = Uri.scheme uri in
  let port =
    match (Uri.port uri, scheme) with
    | Some port, _ -> port
    | _, None | _, Some "http" -> 80
    | _, Some "https" -> 443
    | _ -> failwith "No port or unknown schema in the host"
  in
  let addr =
    Net.getaddrinfo_stream ~service:(string_of_int port) net host |> List.hd
  in

  let start_connection () : connection =
    let socket = Net.connect ~sw net addr in
    let request_stream = Stream.create 0 in

    let request_writer : unit -> Request.t option =
     fun () -> Stream.take request_stream
    in

    let error_promise, error_resolver = Promise.create () in

    let start_stream : stream_runner =
     fun { headers; data_writer; data_receiver; path } ->
      let stream_promise, stream_resolver = Promise.create () in

      let headers =
        Headers.of_list
          [ ("te", headers.te); ("content-type", headers.content_type) ]
      in

      let body_writer : Types.body_writer =
       fun ~window_size:_ ->
        (* TODO: on_flush *)
        let on_flush = ignore in
        match data_writer () with
        | Some cs_l -> (`Data cs_l, on_flush)
        | None -> (`End (None, []), on_flush)
      in

      let error_handler : Error.stream_error -> unit =
       fun (_, code) ->
        Promise.try_resolve stream_resolver
          (Error
             {
               Status_new.code = status_of_h2_error code;
               info =
                 Some
                   (Message
                      (Format.asprintf "HTTP/2 stream error, code %a"
                         Error_code.pp_hum code));
             })
        |> ignore
      in

      let get_grpc_status headers =
        match Headers.get_string headers "grpc-status" with
        | Some status -> (
            match Status_new.code_of_int @@ int_of_string status with
            | OK -> Promise.try_resolve stream_resolver (Ok ())
            | code ->
                Promise.try_resolve stream_resolver
                  (Error { Status_new.code; info = None }))
        | None -> false
      in

      let response_handler : Request.response_handler =
       fun response ->
        get_grpc_status @@ Response.headers response |> ignore;
        let _h2_status = Response.status response in

        let on_data : Types.body_reader = function
          | `Data cs -> data_receiver (Some cs)
          | `End (data_or_not, trailers) ->
              data_receiver data_or_not;
              get_grpc_status trailers |> ignore
        in

        (* TODO: we should have a way in Haha to force close the stream with RST_STREAM NO_ERROR *)
        Response.handle ~on_data
      in

      let request =
        Request.create_with_streaming ?scheme ~authority:(Uri.to_string uri)
          ~error_handler ~headers ~body_writer ~response_handler POST path
      in

      Stream.add request_stream (Some request);

      let result_p, result_r = Promise.create () in

      Fiber.fork ~sw (fun () ->
          Fiber.first
            (fun () -> Promise.resolve result_r (Promise.await stream_promise))
            (fun () ->
              Promise.resolve result_r (Error (Promise.await error_promise))));

      result_p
    in

    let error_handler : Error.connection_error -> unit = function
      | ProtocolError (code, msg) ->
          Promise.resolve error_resolver
            {
              Status_new.code = status_of_h2_error code;
              info = Some (Message msg);
            }
      | Exn exn ->
          Promise.resolve error_resolver
            { Status_new.code = Status_new.Internal; info = Some (Exn exn) }
    in

    let shutdown () =
      if Promise.is_resolved error_promise then ()
      else Stream.add request_stream None
    in

    Fiber.fork ~sw (fun () ->
        Client.run ~request_writer socket |> Result.iter_error error_handler);

    { start_stream; shutdown; open_streams = 0; error_promise }
  in

  let connection_pool : connection_pool = ref [] in

  let start_request : stream_runner =
   fun stream_request ->
    let max_connection =
      List.fold_left
        (fun (acc : connection option) conn ->
          if conn.open_streams >= streams_per_connection then None
          else
            match acc with
            | None -> Some conn
            | Some prev when prev.open_streams >= streams_per_connection -> None
            | Some prev when conn.open_streams > prev.open_streams -> Some conn
            | Some prev -> Some prev)
        None !connection_pool
    in

    match max_connection with
    | Some conn ->
        let res = conn.start_stream stream_request in
        conn.open_streams <- conn.open_streams + 1;

        res
    | None ->
        let conn = start_connection () in
        let res = conn.start_stream stream_request in
        conn.open_streams <- conn.open_streams + 1;
        connection_pool := conn :: !connection_pool;
        res
  in

  let shutdown () =
    List.iter (fun (conn : connection) -> conn.shutdown ()) !connection_pool
  in

  { start_request; shutdown; connection_pool }
