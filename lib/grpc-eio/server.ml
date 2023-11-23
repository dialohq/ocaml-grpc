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
      let rpc = RpcMap.find_opt rpc_name t in
      match rpc with
      | Some rpc -> (
          match rpc with
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

  type t = { protoc_rpc : (module Protoc_rpc.S); rpc : Rpc.t }

  let server ts : server =
    List.fold_left
      (fun map t ->
        let module R = (val t.protoc_rpc) in
        let service_name = Protoc_rpc.service_name (module R) in
        let rpc =
          ServiceMap.find_opt service_name map |> Option.value ~default:[]
        in
        ServiceMap.add service_name (t :: rpc) map)
      ServiceMap.empty ts
    |> ServiceMap.map (fun ts ->
           let service =
             List.fold_left
               (fun acc t ->
                 let module R = (val t.protoc_rpc) in
                 Service.add_rpc
                   ~name:(Protoc_rpc.rpc_name (module R))
                   ~rpc:t.rpc acc)
               (Service.v ()) ts
           in
           Service.handle_request service)

  let encode (type a)
      (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
        with type t = a) (a : a) =
    a |> M.to_proto |> Ocaml_protoc_plugin.Runtime.Runtime'.Writer.contents

  let decode_exn (type a)
      (module M : Ocaml_protoc_plugin.Runtime.Runtime'.Service.Message
        with type t = a) buffer =
    buffer |> Ocaml_protoc_plugin.Runtime.Runtime'.Reader.create |> M.from_proto
    |> function
    | Ok r -> r
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s"
             (Ocaml_protoc_plugin.Result.show_error e))

  let unary (type request response)
      (module Protoc_rpc : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) ~f:handler =
    let handler buffer =
      let status, response =
        handler (decode_exn (module Protoc_rpc.Request) buffer)
      in
      ( status,
        Option.map
          (fun response -> encode (module Protoc_rpc.Response) response)
          response )
    in
    { protoc_rpc = (module Protoc_rpc); rpc = Rpc.Unary handler }

  let server_streaming (type request response)
      (module Protoc_rpc : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) ~f:handler =
    let handler buffer f =
      handler
        (decode_exn (module Protoc_rpc.Request) buffer)
        (fun response -> f (encode (module Protoc_rpc.Response) response))
    in
    { protoc_rpc = (module Protoc_rpc); rpc = Rpc.Server_streaming handler }

  let client_streaming (type request response)
      (module Protoc_rpc : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) ~f:handler =
    let handler requests =
      let requests =
        Seq.map (fun str -> decode_exn (module Protoc_rpc.Request) str) requests
      in
      let status, response = handler requests in
      ( status,
        Option.map
          (fun response -> encode (module Protoc_rpc.Response) response)
          response )
    in
    { protoc_rpc = (module Protoc_rpc); rpc = Rpc.Client_streaming handler }

  let bidirectional_streaming (type request response)
      (module Protoc_rpc : Protoc_rpc.S
        with type Request.t = request
         and type Response.t = response) ~f:handler =
    let handler requests f =
      let requests =
        Seq.map (fun str -> decode_exn (module Protoc_rpc.Request) str) requests
      in
      handler requests (fun response ->
          f (encode (module Protoc_rpc.Response) response))
    in
    {
      protoc_rpc = (module Protoc_rpc);
      rpc = Rpc.Bidirectional_streaming handler;
    }
end
