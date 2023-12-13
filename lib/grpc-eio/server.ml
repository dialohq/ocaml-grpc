module ServiceMap = Map.Make (String)

type service = H2.Reqd.t -> unit
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
    let body = H2.Reqd.request_body reqd in
    let request_reader, request_writer = Seq.create_reader_writer () in
    let response_reader, response_writer = Seq.create_reader_writer () in
    Connection.grpc_recv_streaming body request_writer;
    let status_promise, status_notify = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
        let respond = Seq.write response_writer in
        let status = f request_reader respond in
        Seq.close_writer response_writer;
        Eio.Promise.resolve status_notify status)
      (fun () ->
        try Connection.grpc_send_streaming reqd response_reader status_promise
        with exn ->
          (* https://github.com/anmonteiro/ocaml-h2/issues/175 *)
          Eio.traceln "%s" (Printexc.to_string exn))

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
  module RpcMap = Map.Make (String)

  type t = Rpc.t RpcMap.t

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
      let rpc_impl = RpcMap.find_opt rpc_name t in
      match rpc_impl with
      | Some rpc_impl -> (
          match rpc_impl with
          | Unary f -> Rpc.unary ~f reqd
          | Client_streaming f -> Rpc.client_streaming ~f reqd
          | Server_streaming f -> Rpc.server_streaming ~f reqd
          | Bidirectional_streaming f -> Rpc.bidirectional_streaming ~f reqd)
      | None -> respond_with `Not_found
    else respond_with `Not_found
end

module Typed_rpc = struct
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
        rpc_spec : ('request, 'response, 'service_spec) Grpc.Rpc.Server_rpc.t;
        rpc_impl : Rpc.t;
      }
        -> 'service_spec t

  module Handlers = struct
    type 'service_spec rpc = 'service_spec t

    type t =
      | Handlers : Grpc.Rpc.Service_spec.t rpc list -> t
      | With_service_spec : {
          package : string list;
          service_name : string;
          handlers : unit rpc list;
        }
          -> t
  end

  let server handlers : server =
    let ts =
      match (handlers : Handlers.t) with
      | Handlers ts -> ts
      | With_service_spec { package; service_name; handlers = ts } ->
          List.map
            (fun (T t) ->
              T
                {
                  t with
                  rpc_spec =
                    {
                      t.rpc_spec with
                      service_spec = Some { package; service_name };
                    };
                })
            ts
    in

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
      ServiceMap.empty ts
    |> ServiceMap.map (fun ts ->
           let service =
             List.fold_left
               (fun acc (T t) ->
                 Service.add_rpc ~name:t.rpc_spec.rpc_name ~rpc:t.rpc_impl acc)
               (Service.v ()) ts
           in
           Service.handle_request service)

  let unary (type request response)
      (rpc_spec : (request, response, _) Grpc.Rpc.Server_rpc.t) ~f:handler =
    let handler buffer =
      let status, response = handler (rpc_spec.decode_request buffer) in
      (status, Option.map rpc_spec.encode_response response)
    in
    T { rpc_spec; rpc_impl = Rpc.Unary handler }

  let server_streaming (type request response)
      (rpc_spec : (request, response, _) Grpc.Rpc.Server_rpc.t) ~f:handler =
    let handler buffer f =
      handler (rpc_spec.decode_request buffer) (fun response ->
          f (rpc_spec.encode_response response))
    in
    T { rpc_spec; rpc_impl = Rpc.Server_streaming handler }

  let client_streaming (type request response)
      (rpc_spec : (request, response, _) Grpc.Rpc.Server_rpc.t) ~f:handler =
    let handler requests =
      let requests = Seq.map rpc_spec.decode_request requests in
      let status, response = handler requests in
      (status, Option.map rpc_spec.encode_response response)
    in
    T { rpc_spec; rpc_impl = Rpc.Client_streaming handler }

  let bidirectional_streaming (type request response)
      (rpc_spec : (request, response, _) Grpc.Rpc.Server_rpc.t) ~f:handler =
    let handler requests f =
      let requests = Seq.map rpc_spec.decode_request requests in
      handler requests (fun response -> f (rpc_spec.encode_response response))
    in
    T { rpc_spec; rpc_impl = Rpc.Bidirectional_streaming handler }
end
