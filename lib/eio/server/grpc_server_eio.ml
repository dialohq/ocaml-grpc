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

  type ('net_request, 'req, 'res) handler = {
    headers : 'net_request -> Grpc_server.headers;
    f : 'req Seq.t -> ('res -> unit) -> (string * string) list;
  }

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
  match
    G.parse_request
      ~is_post_request:(Io'.Net_request.is_post request)
      ~get_header:(fun header -> Io'.Net_request.get_header request header)
      ~path:(Io'.Net_request.target request)
    |> Result.map (fun { G.service; meth } -> server ~service ~meth)
  with
  | Ok handler -> (
      let request_stream = Io'.Net_request.body request in
      let { Io.write; write_trailers; close; is_closed } =
        let headers = handler.Rpc.headers request in
        Io'.respond_streaming ~headers request
      in
      try
        let extra = handler.f request_stream write in
        write_trailers (Grpc_server.make_trailers ~extra (Grpc.Status.make OK));
        close ()
      with
      | Server_error (status, extra) ->
          if not (is_closed ()) then (
            write_trailers (Grpc_server.make_trailers ~extra status);
            close ())
      | exn ->
          let extra =
            Option.map (fun f -> f exn) error_handler
            |> Option.value ~default:[]
          in
          if not (is_closed ()) then (
            write_trailers
              (Grpc_server.make_trailers ~extra (Grpc.Status.make Internal));
            close ()))
  | exception Server_error (_status, _extra) ->
      (* FIXME: proper handling *)
      Io'.respond_error request `Not_acceptable
  | Error e -> Io'.respond_error request e
