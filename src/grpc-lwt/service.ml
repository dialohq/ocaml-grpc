module RpcMap = Map.Make (String)

type rpc =
  | Unary of (Pbrt.Decoder.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t)
  | Client_streaming of
      (Pbrt.Decoder.t Lwt_stream.t ->
      (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t)
  | Server_streaming of
      (Pbrt.Decoder.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t)
  | Bidirectional_streaming of
      (Pbrt.Decoder.t Lwt_stream.t ->
      (Pbrt.Encoder.t -> unit) ->
      Grpc.Status.t Lwt.t)

type t = rpc RpcMap.t

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
        | Unary f -> Lwt.async (fun () -> Rpc.unary ~f ~reqd)
        | Client_streaming f ->
            Lwt.async (fun () -> Rpc.client_streaming ~f ~reqd)
        | Server_streaming f ->
            Lwt.async (fun () -> Rpc.server_streaming ~f ~reqd)
        | Bidirectional_streaming f ->
            Lwt.async (fun () -> Rpc.bidirectional_streaming ~f ~reqd) )
    | None -> respond_with `Not_found
  else respond_with `Not_found
