open Eio
module H2 = Haha
module Client = Haha.Client

let find_grpc_status headers =
  H2.Header.find_opt "grpc-status" headers
  |> Option.map @@ fun status ->
     let code = Status.code_of_int @@ int_of_string status in
     {
       Status.code;
       info =
         H2.Header.find_opt "grpc-message" headers
         |> Option.map (fun s -> Status.Message s);
     }

let status_of_h2_error : H2.Error_code.t -> Status.code = function
  (* NO_ERROR case of early shutdown *)
  | NoError | StreamClosed | Cancel -> Cancelled
  | ProtocolError | InternalError | FrameSizeError | CompressionError
  | ConnectError | InadequateSecurity | HTTP_1_1_Required ->
      Internal
  | FlowControlError | EnhanceYourCalm -> Resource_exhausted
  | SettingsTimeout | RefusedStream | UnknownError_code _ -> Unavailable

let connection_error_to_status : H2.Error.connection_error -> Status.t =
  function
  | ProtocolViolation (code, msg) | PeerError (code, msg) ->
      { Status.code = status_of_h2_error code; info = Some (Message msg) }
  | Exn exn -> { Status.code = Internal; info = Some (Exn exn) }

type 'context data_receiver_result = {
  action : [ `Continue | `Reset ];
  context : 'context;
}

type 'context data_receiver =
  'context -> Cstruct.t option -> 'context data_receiver_result

type 'context data_writer = 'context -> Cstruct.t list option * 'context
type 'c stream_result = { status : Status.t; grpc_context : 'c }

type 'c stream_request = {
  headers : Utils.request_headers;
  data_writer : 'c data_writer;
  data_receiver : 'c data_receiver;
  path : string;
  result_resolver : 'c stream_result Promise.u;
  initial_grpc_context : 'c;
}

type 'c stream_context = {
  result : Status.t option;
  resolver : 'c stream_result Promise.u; [@opaque]
  grpc_context : 'c; [@opaque]
}
[@@deriving show]

type 'c connection = {
  next_iter : unit -> 'c stream_context Client.iteration;
  open_streams : int;
  start_stream : 'c stream_request -> unit;
  shutdown : unit -> unit;
}
[@@deriving show]

type 'c state = { connection_pool : 'c connection list; shutdown : bool }
[@@deriving show]

type 'c t = {
  request_stream : 'c stream_request Stream.t;
  shutdown_resolver : unit Promise.u;
}

let stream_error_handler :
    _ stream_context -> H2.Error_code.t -> _ stream_context =
 fun context code ->
  {
    context with
    result =
      Some
        {
          Status.code = status_of_h2_error code;
          info =
            Some
              (Message
                 (Format.asprintf "HTTP/2 stream error, code %a"
                    H2.Error_code.pp_hum code));
        };
  }

let resolve_streams :
    ?err:H2.Error.connection_error -> (int32 * _ stream_context) list -> unit =
 fun ?err closed_ctxs ->
  List.iter
    (fun (_, { grpc_context; result; resolver }) ->
      (match (result, err) with
      | None, None ->
          {
            grpc_context;
            status =
              {
                code = Unknown;
                info =
                  Some
                    (Message
                       "gRPC protocol error: No gRPC status found in the \
                        HTTP/2 response trailers");
              };
          }
      | Some status, _ -> { status; grpc_context }
      | None, Some (ProtocolViolation (code, msg) | PeerError (code, msg)) ->
          {
            grpc_context;
            status =
              {
                code = status_of_h2_error code;
                info =
                  Some
                    (Message
                       (Format.asprintf "HTTP/2 connection error, code %a: %s"
                          H2.Error_code.pp_hum code msg));
              };
          }
      | None, Some (Exn exn) ->
          { grpc_context; status = { code = Internal; info = Some (Exn exn) } })
      |> Promise.resolve resolver)
    closed_ctxs

let make_connections_events :
    int -> _ connection -> unit -> _ state -> _ state option =
 fun idx conn () ->
  let iteration = conn.next_iter () in

  fun state ->
    let new_pool =
      match iteration.state with
      | End ->
          resolve_streams iteration.closed_ctxs;
          List.filteri (fun i _ -> i <> idx) state.connection_pool
      | Error err ->
          resolve_streams ~err iteration.closed_ctxs;
          List.filteri (fun i _ -> i <> idx) state.connection_pool
      | InProgress next_iter ->
          resolve_streams iteration.closed_ctxs;
          List.mapi
            (fun i conn ->
              if i = idx then (
                let new_conn =
                  {
                    conn with
                    next_iter;
                    open_streams =
                      conn.open_streams - List.length iteration.closed_ctxs;
                  }
                in

                if
                  new_conn.open_streams < 1
                  && List.length state.connection_pool > 1
                then new_conn.shutdown ();
                new_conn)
              else conn)
            state.connection_pool
    in

    if List.is_empty new_pool then None
    else Some { state with connection_pool = new_pool }

let make_request : Uri.t -> _ stream_request -> _ stream_context H2.Request.t =
 fun uri
     {
       headers;
       data_writer;
       data_receiver;
       path;
       result_resolver;
       initial_grpc_context;
     } ->
  let headers =
    H2.Header.of_list
      [ ("te", headers.te); ("content-type", headers.content_type) ]
  in

  let body_writer : _ stream_context H2.Types.body_writer =
   fun context ~window_size:_ ->
    let data, grpc_context = data_writer context.grpc_context in
    match data with
    | Some cs_l -> (`Data cs_l, ignore, { context with grpc_context })
    | None -> (`End (None, []), ignore, { context with grpc_context })
  in

  let on_data : _ stream_context H2.Types.body_reader =
   fun context -> function
    | `Data cs ->
        let { context = grpc_context; action } =
          data_receiver context.grpc_context (Some cs)
        in
        { action; context = { context with grpc_context } }
    | `End (cs_opt, trailers) -> (
        let { context = grpc_context; action } =
          data_receiver context.grpc_context cs_opt
        in

        match find_grpc_status trailers with
        | Some status ->
            {
              action;
              context = { context with grpc_context; result = Some status };
            }
        | None -> { action; context = { context with grpc_context } })
  in

  let response_handler : _ stream_context H2.Response.handler =
   fun context response ->
    match H2.Response.status response with
    | `OK ->
        let context =
          {
            context with
            result = find_grpc_status (H2.Response.headers response);
          }
        in

        (Some on_data, context)
    | h2_status ->
        let context =
          {
            context with
            result =
              Some
                {
                  Status.code = Internal;
                  info =
                    Some
                      (Message
                         (Format.sprintf
                            "gRPC protocol error: HTTP/2 server responsed with \
                             %i status code, 200 expected"
                            (Haha.Status.to_code h2_status)));
                };
          }
        in

        (None, context)
  in

  let initial_stream_state =
    {
      result = None;
      resolver = result_resolver;
      grpc_context = initial_grpc_context;
    }
  in

  H2.Request.create_with_streaming ~context:initial_stream_state
    ?scheme:(Uri.scheme uri) ~authority:(Uri.to_string uri) ~headers
    ~error_handler:stream_error_handler ~response_handler ~body_writer POST path

let start_connection :
    connect_socket:(unit -> _ Net.stream_socket) ->
    uri:Uri.t ->
    (_ connection, H2.Error.connection_error) result =
 fun ~connect_socket ~uri ->
  let socket = connect_socket () in

  let request_stream : _ stream_context H2.Request.t option Stream.t =
    Stream.create max_int
  in

  let request_writer : unit -> _ stream_context H2.Request.t option =
   fun () -> Stream.take request_stream
  in

  let shutdown () = Stream.add request_stream None in

  let start_stream stream_request =
    Stream.add request_stream (Some (make_request uri stream_request))
  in

  let initial_iteration = Client.connect ~request_writer socket in

  match initial_iteration with
  | { state = End; _ } as iter ->
      Ok
        {
          next_iter = (fun () -> iter);
          open_streams = 1;
          start_stream;
          shutdown;
        }
  | { state = Error err; _ } -> Error err
  | { state = InProgress next_iter; _ } ->
      Ok { next_iter; open_streams = 1; start_stream; shutdown }

let make_new_stream_event :
    new_connection:(unit -> (_ connection, H2.Error.connection_error) result) ->
    max_streams:int ->
    request_stream:_ stream_request Stream.t ->
    unit ->
    _ state ->
    _ state option =
 fun ~new_connection ~max_streams ~request_stream () ->
  let stream_request = Stream.take request_stream in

  fun state ->
    let max_connection =
      List.fold_left
        (fun (max_valid : (_ connection * int) option) (idx, conn) ->
          match max_valid with
          | None when conn.open_streams < max_streams -> Some (conn, idx)
          | None -> None
          | Some (prev, _)
            when conn.open_streams < max_streams
                 && conn.open_streams > prev.open_streams ->
              Some (conn, idx)
          | Some prev -> Some prev)
        None
        (List.mapi (fun i c -> (i, c)) state.connection_pool)
    in

    match max_connection with
    | Some (conn, idx) ->
        conn.start_stream stream_request;
        Some
          {
            state with
            connection_pool =
              List.mapi
                (fun i conn ->
                  if i = idx then
                    { conn with open_streams = conn.open_streams + 1 }
                  else conn)
                state.connection_pool;
          }
    | None -> (
        match new_connection () with
        | Ok connection ->
            connection.start_stream stream_request;

            Some
              {
                state with
                connection_pool = connection :: state.connection_pool;
              }
        | Error conn_err ->
            Promise.resolve stream_request.result_resolver
              {
                status = connection_error_to_status conn_err;
                grpc_context = stream_request.initial_grpc_context;
              };
            Some state)

let make_shutdown_event :
    shutdown_promise:unit Promise.t -> unit -> _ state -> _ state option =
 fun ~shutdown_promise () ->
  Promise.await shutdown_promise;
  fun state ->
    List.iter
      (fun (conn : _ connection) -> conn.shutdown ())
      state.connection_pool;
    Some { state with shutdown = true }

let create : ?max_streams:int -> sw:Switch.t -> net:_ Net.t -> string -> _ t =
 fun ?(max_streams = 100) ~sw ~net uri ->
  let uri = Uri.of_string uri in
  let request_stream : _ stream_request Stream.t = Stream.create 0 in
  let shutdown_promise, shutdown_resolver = Promise.create () in

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
  let connect_socket () =
    Net.(
      connect ~sw net
        (getaddrinfo_stream ~service:(string_of_int port) net host |> List.hd))
  in

  Fiber.fork ~sw (fun () ->
      let rec runloop state =
        let new_connection () = start_connection ~connect_socket ~uri in
        let new_stream_event =
          make_new_stream_event ~new_connection ~max_streams ~request_stream
        in
        let shutdown_event = make_shutdown_event ~shutdown_promise in
        let connections_events =
          List.mapi make_connections_events state.connection_pool
        in

        let f =
          Fiber.any
            (if state.shutdown then new_stream_event :: connections_events
             else shutdown_event :: new_stream_event :: connections_events)
        in
        Option.iter (fun state -> runloop state) (f state)
      in

      runloop { connection_pool = []; shutdown = false });

  { request_stream; shutdown_resolver }

let start_request (t : _ t) ~headers ~data_writer ~data_receiver ~path
    ~initial_context =
  let result_promise, result_resolver = Promise.create () in

  Stream.add t.request_stream
    {
      result_resolver;
      headers;
      data_receiver;
      data_writer;
      path;
      initial_grpc_context = initial_context;
    };

  result_promise

let shutdown (t : _ t) =
  Printf.printf "Shutting down the channel\n%!";
  if not (Promise.try_resolve t.shutdown_resolver ()) then
    failwith "cannot shutdown channel twice"
