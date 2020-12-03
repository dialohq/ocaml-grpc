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
                    else respond_with `Not_acceptable )
            | Some _ ->
                (* TODO: not sure if there is a specific way to handle this in grpc *)
                respond_with `Bad_request
          else respond_with `Unsupported_media_type
      | None -> respond_with `Unsupported_media_type )
  | _ -> respond_with `Not_found

module Rpc = struct
  open Lwt.Syntax

  type unary = Grpc.Buffer.t -> (Grpc.Status.t * string option) Lwt.t

  type client_streaming =
    Grpc.Buffer.t Lwt_stream.t -> (Grpc.Status.t * string option) Lwt.t

  type server_streaming =
    Grpc.Buffer.t -> (string -> unit) -> Grpc.Status.t Lwt.t

  type bidirectional_streaming =
    Grpc.Buffer.t Lwt_stream.t -> (string -> unit) -> Grpc.Status.t Lwt.t

  type t =
    | Unary of unary
    | Client_streaming of client_streaming
    | Server_streaming of server_streaming
    | Bidirectional_streaming of bidirectional_streaming

  let bidirectional_streaming ~f reqd =
    let decoder_stream, decoder_push = Lwt_stream.create () in
    let body = H2.Reqd.request_body reqd in
    Connection.grpc_recv_streaming body decoder_push;
    let encoder_stream, encoder_push = Lwt_stream.create () in
    let status_mvar = Lwt_mvar.create_empty () in
    Lwt.async (fun () ->
        Connection.grpc_send_streaming reqd encoder_stream status_mvar);
    let* status =
      f decoder_stream (fun encoder -> encoder_push (Some encoder))
    in
    encoder_push None;
    Lwt_mvar.put status_mvar status

  let client_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let+ status, encoder = f decoder_stream in
        (match encoder with None -> () | Some encoder -> encoder_push encoder);
        (status : Grpc.Status.t))

  let server_streaming ~f reqd =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let* decoder = Lwt_stream.get decoder_stream in
        match decoder with
        | None -> Lwt.return Grpc.Status.(v OK)
        | Some decoder -> f decoder encoder_push)

  let unary ~f reqd =
    bidirectional_streaming reqd ~f:(fun decoder_stream encoder_push ->
        let* decoder = Lwt_stream.get decoder_stream in
        match decoder with
        | None -> Lwt.return Grpc.Status.(v OK)
        | Some decoder ->
            let+ status, encoder = f decoder in
            ( match encoder with
            | None -> ()
            | Some encoder -> encoder_push encoder );
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
          | Unary f -> Lwt.async (fun () -> Rpc.unary ~f reqd)
          | Client_streaming f ->
              Lwt.async (fun () -> Rpc.client_streaming ~f reqd)
          | Server_streaming f ->
              Lwt.async (fun () -> Rpc.server_streaming ~f reqd)
          | Bidirectional_streaming f ->
              Lwt.async (fun () -> Rpc.bidirectional_streaming ~f reqd) )
      | None -> respond_with `Not_found
    else respond_with `Not_found
end
