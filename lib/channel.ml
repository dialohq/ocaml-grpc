open Eio
module H2 = Haha
module Client = Haha.Client

let find_grpc_status headers =
  H2.Headers.find_opt "grpc-status" headers
  |> Option.map @@ fun status ->
     let code = Status.code_of_int @@ int_of_string status in
     {
       Status.code;
       info =
         H2.Headers.find_opt "grpc-message" headers
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

type 'context data_receiver = 'context -> Cstruct.t option -> 'context
type 'context data_writer = 'context -> Cstruct.t list option * 'context
type 'c stream_result = { status : Status.t; grpc_context : 'c }

type 'c stream_context = {
  result : Status.t option;
  resolver : 'c stream_result Promise.u; [@opaque]
  grpc_context : 'c; [@opaque]
}
[@@deriving show]

type connection = {
  next_iter : Client.iter_input list -> Client.iteration;
  open_streams : int;
  shutdown : bool;
  pending_inputs : Client.iter_input list;
      [@printer fun fmt l -> fprintf fmt "<%i inputs>" (List.length l)]
}
[@@deriving show]

type state = { connection_pool : connection list; shutdown : bool }
[@@deriving show]

type transition = state -> state option
type event = unit -> transition

type t = {
  uri : Uri.t;
  request_stream : (H2.Request.t * (Status.t -> unit)) Stream.t;
  shutdown_resolver : unit Promise.u;
}

let search_connections ~max_streams connection_pool =
  List.fold_left
    (fun (max_valid : (connection * int) option) (idx, conn) ->
      match max_valid with
      | None when conn.open_streams < max_streams -> Some (conn, idx)
      | None -> None
      | Some (prev, _)
        when conn.open_streams < max_streams
             && conn.open_streams > prev.open_streams ->
          Some (conn, idx)
      | Some prev -> Some prev)
    None
    (List.mapi (fun i c -> (i, c)) connection_pool)

let shutdown_connection : connection -> connection =
 fun c ->
  if c.shutdown then c
  else
    { c with shutdown = true; pending_inputs = c.pending_inputs @ [ Shutdown ] }

let start_request : connection -> H2.Request.t -> connection =
 fun c req ->
  if c.shutdown then c
  else { c with pending_inputs = c.pending_inputs @ [ Request req ] }

let combine : transition -> transition -> transition =
 fun x y state -> Option.bind (x state) y

let stream_error_handler : _ stream_context -> H2.Error.t -> _ stream_context =
 fun context -> function
  | StreamError (_, code) ->
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
  | ConnectionError (Exn exn) ->
      { context with result = Some { code = Internal; info = Some (Exn exn) } }
  | ConnectionError (ProtocolViolation (code, msg) | PeerError (code, msg)) ->
      {
        context with
        result =
          Some
            {
              code = status_of_h2_error code;
              info =
                Some
                  (Message
                     (Format.asprintf "HTTP/2 connection error, code %a: %s"
                        H2.Error_code.pp_hum code msg));
            };
      }

let make_connections_event : int -> connection -> event =
 fun idx conn () ->
  let iteration = conn.next_iter conn.pending_inputs in

  fun state ->
    let new_pool =
      match iteration.state with
      | End -> List.filteri (fun i _ -> i <> idx) state.connection_pool
      | Error _ -> List.filteri (fun i _ -> i <> idx) state.connection_pool
      | InProgress next_iter ->
          List.mapi
            (fun i conn ->
              if i = idx then
                let new_conn =
                  {
                    conn with
                    next_iter;
                    pending_inputs = [];
                    open_streams = iteration.active_streams;
                  }
                in

                if
                  new_conn.open_streams < 1
                  && List.length state.connection_pool > 1
                then shutdown_connection new_conn
                else new_conn
              else conn)
            state.connection_pool
    in

    if List.is_empty new_pool && state.shutdown then None
    else Some { state with connection_pool = new_pool }

let make_request :
    uri:Uri.t ->
    headers:(string * string) list ->
    path:string ->
    data_writer:'a data_writer ->
    data_receiver:'a data_receiver ->
    result_resolver:'c stream_result Promise.u ->
    initial_grpc_context:'a ->
    H2.Request.t =
 fun ~uri ~headers ~path ~data_writer ~data_receiver ~result_resolver
     ~initial_grpc_context ->
  let body_writer : _ stream_context H2.Body.writer =
   fun context ->
    let data, grpc_context = data_writer context.grpc_context in
    match (context.result, data) with
    | None, Some cs_l ->
        {
          payload = `Data cs_l;
          on_flush = ignore;
          context = { context with grpc_context };
        }
    | None, None ->
        {
          payload = `End (None, H2.Headers.empty);
          on_flush = ignore;
          context = { context with grpc_context };
        }
    | Some _, cs_opt ->
        {
          payload = `End (cs_opt, H2.Headers.empty);
          on_flush = ignore;
          context = { context with grpc_context };
        }
  in

  let on_data : _ stream_context H2.Body.reader =
   fun context -> function
    | `Data cs ->
        let grpc_context = data_receiver context.grpc_context (Some cs) in
        { context with grpc_context }
    | `End trailers -> (
        let grpc_context = data_receiver context.grpc_context None in

        match find_grpc_status trailers with
        | Some status -> { context with grpc_context; result = Some status }
        | None -> { context with grpc_context })
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

  let on_close : _ stream_context -> unit =
   fun { result; grpc_context; resolver } ->
    let unknown_result =
      {
        grpc_context;
        status =
          {
            code = Unknown;
            info =
              Some
                (Message
                   "gRPC protocol error: No gRPC status found in the HTTP/2 \
                    response trailers");
          };
      }
    in

    match result with
    | Some status -> Promise.resolve resolver { status; grpc_context }
    | None -> Promise.resolve resolver unknown_result
  in

  let initial_stream_state =
    {
      result = None;
      resolver = result_resolver;
      grpc_context = initial_grpc_context;
    }
  in

  let headers = H2.Headers.of_list headers in
  H2.Request.create_with_streaming ~context:initial_stream_state
    ?scheme:(Uri.scheme uri) ?authority:(Uri.host uri) ~headers
    ~error_handler:stream_error_handler ~on_close ~response_handler ~body_writer
    POST path

let start_connection :
    connect_socket:(unit -> (_ Net.stream_socket, exn) result) ->
    (connection, [ `H2Error of H2.Error.connection_error | `Exn of exn ]) result
    =
 fun ~connect_socket ->
  match connect_socket () with
  | Error exn -> Error (`Exn exn)
  | Ok socket -> (
      let initial_iteration = Client.connect socket in

      match initial_iteration with
      | { state = End; _ } as iter ->
          Ok
            {
              next_iter = (fun _ -> iter);
              open_streams = 0;
              pending_inputs = [];
              shutdown = false;
            }
      | { state = Error err; _ } -> Error (`H2Error err)
      | { state = InProgress next_iter; _ } ->
          Ok
            {
              next_iter;
              open_streams = 0;
              shutdown = false;
              pending_inputs = [];
            })

let make_new_stream_event :
    new_connection:
      (unit ->
      ( connection,
        [ `H2Error of H2.Error.connection_error | `Exn of exn ] )
      result) ->
    max_streams:int ->
    request_stream:(H2.Request.t * (Status.t -> unit)) Stream.t ->
    unit ->
    state ->
    state option =
 fun ~new_connection ~max_streams ~request_stream () ->
  let request, on_init_error = Stream.take request_stream in

  fun state ->
    match search_connections ~max_streams state.connection_pool with
    | Some (conn, idx) ->
        let new_conn = start_request conn request in
        Some
          {
            state with
            connection_pool =
              List.mapi
                (fun i conn -> if i = idx then new_conn else conn)
                state.connection_pool;
          }
    | None -> (
        match new_connection () with
        | Ok connection ->
            let new_conn = start_request connection request in

            Some
              { state with connection_pool = new_conn :: state.connection_pool }
        | Error (`H2Error conn_err) ->
            on_init_error (connection_error_to_status conn_err);
            Some state
        | Error (`Exn exn) ->
            on_init_error { code = Unavailable; info = Some (Exn exn) };
            Some state)

let make_shutdown_event : shutdown_promise:unit Promise.t -> event =
 fun ~shutdown_promise () ->
  Promise.await shutdown_promise;
  fun state ->
    let connection_pool = List.map shutdown_connection state.connection_pool in
    Some { shutdown = true; connection_pool }

let create : ?max_streams:int -> sw:Switch.t -> net:_ Net.t -> string -> t =
 fun ?(max_streams = 100) ~sw ~net uri ->
  let uri = Uri.of_string uri in
  let request_stream : (H2.Request.t * (Status.t -> unit)) Stream.t =
    Stream.create 0
  in
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
    try
      Ok
        Net.(
          connect ~sw net
            (getaddrinfo_stream ~service:(string_of_int port) net host
            |> List.hd))
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Error exn
  in

  Fiber.fork ~sw (fun () ->
      let rec runloop state =
        let new_connection () = start_connection ~connect_socket in

        let new_stream_event : event =
          make_new_stream_event ~new_connection ~max_streams ~request_stream
        in
        let shutdown_event : event = make_shutdown_event ~shutdown_promise in
        let connections_events : event list =
          List.mapi make_connections_event state.connection_pool
        in

        let transition : transition =
          match (state.shutdown, connections_events) with
          | true, [] -> fun _ -> None
          | true, _ ->
              Fiber.any ~combine (new_stream_event :: connections_events)
          | _ ->
              Fiber.any ~combine
                (shutdown_event :: new_stream_event :: connections_events)
        in

        Option.iter runloop (transition state)
      in

      runloop { connection_pool = []; shutdown = false });

  { request_stream; shutdown_resolver; uri }

let start_request { request_stream; uri; _ } ~headers ~data_writer
    ~data_receiver ~path ~initial_context =
  let result_promise, result_resolver = Promise.create () in

  let on_init_error status =
    Promise.resolve result_resolver { status; grpc_context = initial_context }
  in

  Stream.add request_stream
    ( make_request ~uri ~result_resolver ~path ~headers ~data_receiver
        ~data_writer ~initial_grpc_context:initial_context,
      on_init_error );

  result_promise

let shutdown (t : t) =
  if not (Promise.try_resolve t.shutdown_resolver ()) then
    failwith "cannot shutdown channel twice"
