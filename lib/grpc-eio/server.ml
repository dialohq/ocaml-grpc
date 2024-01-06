module ServiceMap = Map.Make (String)

type reqd_handler = H2.Reqd.t -> unit
type service = reqd_handler
type t = service ServiceMap.t

let v () = ServiceMap.empty
let add_service ~name ~service t = ServiceMap.add name service t

let handle_request t reqd =
  let request = H2.Reqd.request reqd in
  let respond_with code =
    H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
  in
  let route () =
    let parts = String.split_on_char '/' request.target in
    if List.length parts > 1 then
      (* allow for arbitrary prefixes *)
      let service_name = List.nth parts (List.length parts - 2) in
      let service = ServiceMap.find_opt service_name t in
      match service with
      | Some service -> service reqd
      | None -> respond_with `Not_found
    else respond_with `Not_found
  in
  match request.meth with
  | `POST -> (
      match H2.Headers.get request.headers "content-type" with
      | Some s ->
          if
            Stringext.chop_prefix s ~prefix:"application/grpc" |> Option.is_some
          then
            match H2.Headers.get request.headers "grpc-encoding" with
            | None | Some "identity" -> (
                match H2.Headers.get request.headers "grpc-accept-encoding" with
                | None -> route ()
                | Some encodings ->
                    let encodings = String.split_on_char ',' encodings in
                    if List.mem "identity" encodings then route ()
                    else respond_with `Not_acceptable)
            | Some _ ->
                (* TODO: not sure if there is a specific way to handle this in grpc *)
                respond_with `Bad_request
          else respond_with `Unsupported_media_type
      | None -> respond_with `Unsupported_media_type)
  | _ -> respond_with `Not_found

let implement_rpc ~decode_request ~encode_response ~f reqd =
  let body = H2.Reqd.request_body reqd in
  let request_reader, request_writer = Seq.create_reader_writer () in
  let response_reader, response_writer = Seq.create_reader_writer () in
  Connection.grpc_recv_streaming ~decode:decode_request body request_writer;
  let status_promise, status_notify = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () ->
      let respond = Seq.write response_writer in
      let status = f request_reader respond in
      Seq.close_writer response_writer;
      Eio.Promise.resolve status_notify status)
    (fun () ->
      try
        Connection.grpc_send_streaming ~encode:encode_response reqd
          response_reader status_promise
      with exn ->
        (* https://github.com/anmonteiro/ocaml-h2/issues/175 *)
        Eio.traceln "%s" (Printexc.to_string exn))

module Typed_rpc = struct
  module Service = struct
    module RpcMap = Map.Make (String)

    type t = reqd_handler RpcMap.t

    let v () = RpcMap.empty
    let add_rpc ~name ~rpc t = RpcMap.add name rpc t

    let handle_request (t : t) reqd =
      let request = H2.Reqd.request reqd in
      let respond_with code =
        H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
      in
      let parts = String.split_on_char '/' request.target in
      if List.length parts > 1 then
        let rpc_name = List.nth parts (List.length parts - 1) in
        match RpcMap.find_opt rpc_name t with
        | Some rpc -> rpc reqd
        | None -> respond_with `Not_found
      else respond_with `Not_found
  end

  type server = t

  type ('request, 'response) unary =
    'request -> Grpc.Status.t * 'response option

  type ('request, 'response) client_streaming =
    'request Seq.t -> Grpc.Status.t * 'response option

  type ('request, 'response) server_streaming =
    'request -> ('response -> unit) -> Grpc.Status.t

  type ('request, 'response) bidirectional_streaming =
    'request Seq.t -> ('response -> unit) -> Grpc.Status.t

  type 'service_spec t =
    | T : {
        rpc_spec :
          ( 'request,
            'request_mode,
            'response,
            'response_mode,
            'service_spec )
          Grpc.Rpc.Server_rpc.t;
        rpc_impl : reqd_handler;
      }
        -> 'service_spec t

  let rec make_handlers handlers =
    match (handlers : _ Grpc.Rpc.Handlers.t) with
    | a :: tl -> List.concat (make_handlers a :: List.map make_handlers tl)
    | Handlers { handlers = ts } -> ts
    | With_service_spec { service_spec; handlers = ts } ->
        List.map
          (fun (T t) ->
            T
              {
                t with
                rpc_spec = { t.rpc_spec with service_spec = Some service_spec };
              })
          ts

  let server handlers : server =
    let handlers = make_handlers handlers in
    List.fold_left
      (fun map (T t as packed) ->
        let service_name =
          match t.rpc_spec.service_spec with
          | Some service_spec ->
              Grpc.Rpc.Service_spec.packaged_service_name service_spec
        in
        let rpc_impl =
          ServiceMap.find_opt service_name map |> Option.value ~default:[]
        in
        ServiceMap.add service_name (packed :: rpc_impl) map)
      ServiceMap.empty handlers
    |> ServiceMap.map (fun ts ->
           let service =
             List.fold_left
               (fun acc (T t) ->
                 Service.add_rpc ~name:t.rpc_spec.rpc_name ~rpc:t.rpc_impl acc)
               (Service.v ()) ts
           in
           Service.handle_request service)

  let implement_rpc (type request response)
      ~(rpc_spec : (request, _, response, _, _) Grpc.Rpc.Server_rpc.t) ~f =
    let rpc_impl =
      implement_rpc ~decode_request:rpc_spec.decode_request
        ~encode_response:rpc_spec.encode_response ~f
    in
    T { rpc_spec; rpc_impl }

  let bidirectional_streaming (type request response)
      (rpc_spec :
        ( request,
          Grpc.Rpc.Value_mode.stream,
          response,
          Grpc.Rpc.Value_mode.stream,
          _ )
        Grpc.Rpc.Server_rpc.t) ~f =
    implement_rpc ~rpc_spec ~f

  let unary (type request response)
      (rpc_spec :
        ( request,
          Grpc.Rpc.Value_mode.unary,
          response,
          Grpc.Rpc.Value_mode.unary,
          _ )
        Grpc.Rpc.Server_rpc.t) ~f =
    implement_rpc ~rpc_spec ~f:(fun requests respond ->
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request ->
            let status, response = f request in
            (match response with
            | None -> ()
            | Some response -> respond response);
            status)

  let server_streaming (type request response)
      (rpc_spec :
        ( request,
          Grpc.Rpc.Value_mode.unary,
          response,
          Grpc.Rpc.Value_mode.stream,
          _ )
        Grpc.Rpc.Server_rpc.t) ~f =
    implement_rpc ~rpc_spec ~f:(fun requests respond ->
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request -> f request respond)

  let client_streaming (type request response)
      (rpc_spec :
        ( request,
          Grpc.Rpc.Value_mode.stream,
          response,
          Grpc.Rpc.Value_mode.unary,
          _ )
        Grpc.Rpc.Server_rpc.t) ~f =
    implement_rpc ~rpc_spec ~f:(fun requests respond ->
        let status, response = f requests in
        (match response with None -> () | Some response -> respond response);
        status)
end

module Rpc = struct
  type unary = string -> Grpc.Status.t * string option
  type client_streaming = string Seq.t -> Grpc.Status.t * string option
  type server_streaming = string -> (string -> unit) -> Grpc.Status.t

  type bidirectional_streaming =
    string Seq.t -> (string -> unit) -> Grpc.Status.t

  type t =
    | Unary of unary
    | Client_streaming of client_streaming
    | Server_streaming of server_streaming
    | Bidirectional_streaming of bidirectional_streaming

  let bidirectional_streaming ~f reqd =
    implement_rpc ~decode_request:Fun.id ~encode_response:Fun.id ~f reqd

  let client_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun requests respond ->
        let status, response = f requests in
        (match response with None -> () | Some response -> respond response);
        status)

  let server_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun requests respond ->
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request -> f request respond)

  let unary ~f reqd =
    bidirectional_streaming reqd ~f:(fun requests respond ->
        match Seq.read_and_exhaust requests with
        | None -> Grpc.Status.(v OK)
        | Some request ->
            let status, response = f request in
            (match response with
            | None -> ()
            | Some response -> respond response);
            status)
end

module Service = struct
  include Typed_rpc.Service

  let add_rpc ~name ~rpc t =
    add_rpc ~name
      ~rpc:
        (match rpc with
        | Rpc.Unary f -> Rpc.unary ~f
        | Client_streaming f -> Rpc.client_streaming ~f
        | Server_streaming f -> Rpc.server_streaming ~f
        | Bidirectional_streaming f -> Rpc.bidirectional_streaming ~f)
      t
end
