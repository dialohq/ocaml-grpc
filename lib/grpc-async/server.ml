open! Async
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
  type unary = string -> (Grpc.Status.t * string option) Deferred.t

  type client_streaming =
    string Pipe.Reader.t -> (Grpc.Status.t * string option) Deferred.t

  type server_streaming =
    string -> string Pipe.Writer.t -> Grpc.Status.t Deferred.t

  type bidirectional_streaming =
    string Pipe.Reader.t -> string Pipe.Writer.t -> Grpc.Status.t Deferred.t

  type t =
    | Unary of unary
    | Client_streaming of client_streaming
    | Server_streaming of server_streaming
    | Bidirectional_streaming of bidirectional_streaming

  let bidirectional_streaming ~(f : bidirectional_streaming) (reqd : H2.Reqd.t)
      : unit Deferred.t =
    let decoder_stream, decoder_push = Async.Pipe.create () in
    let body = H2.Reqd.request_body reqd in

    (* Pass the H2 body reader and the push function to grpc_recv_streaming. *)
    Connection.grpc_recv_streaming body decoder_push;

    (* Create outgoing string Pipe.t. *)
    let encoder_stream, encoder_push = Async.Pipe.create () in

    (* Signal MVar for comms between receiving and sending. *)
    let status_mvar = Async.Mvar.create () in

    don't_wait_for
      (Connection.grpc_send_streaming reqd encoder_stream status_mvar);
    let%bind status = f decoder_stream encoder_push in
    if not (Pipe.is_closed encoder_push) then Pipe.close encoder_push;
    if not (Pipe.is_closed decoder_push) then Pipe.close decoder_push;
    Mvar.put status_mvar status

  let client_streaming ~(f : client_streaming) (reqd : H2.Reqd.t) =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let%bind status, encoder = f decoder_stream in
        let%bind () =
          match encoder with
          | Some encoder -> Pipe.write encoder_push encoder
          | None -> return ()
        in
        return status)

  let server_streaming ~(f : server_streaming) (reqd : H2.Reqd.t) =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let%bind decoder = Pipe.read decoder_stream in
        match decoder with
        | `Eof -> return Grpc.Status.(v OK)
        | `Ok decoder -> f decoder encoder_push)

  let unary ~(f : unary) (reqd : H2.Reqd.t) =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let%bind decoder = Pipe.read decoder_stream in
        match decoder with
        | `Eof -> return Grpc.Status.(v OK)
        | `Ok decoder ->
            let%bind status, encoder = f decoder in
            let%bind () =
              match encoder with
              | None -> return ()
              | Some encoder -> Pipe.write encoder_push encoder
            in
            return status)
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
          | Unary f -> don't_wait_for (Rpc.unary ~f reqd)
          | Client_streaming f -> don't_wait_for (Rpc.client_streaming ~f reqd)
          | Server_streaming f -> don't_wait_for (Rpc.server_streaming ~f reqd)
          | Bidirectional_streaming f ->
              don't_wait_for (Rpc.bidirectional_streaming ~f reqd))
      | None -> respond_with `Not_found
    else respond_with `Not_found
end
