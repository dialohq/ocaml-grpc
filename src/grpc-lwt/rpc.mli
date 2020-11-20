type unary = Pbrt.Decoder.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t

type client_streaming =
  Pbrt.Decoder.t Lwt_stream.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t

type server_streaming =
  Pbrt.Decoder.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t

type bidirectional_streaming =
  Pbrt.Decoder.t Lwt_stream.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t

type t =
  | Unary of unary
  | Client_streaming of client_streaming
  | Server_streaming of server_streaming
  | Bidirectional_streaming of bidirectional_streaming

val unary : f:unary -> reqd:H2.Reqd.t -> unit Lwt.t

val client_streaming : f:client_streaming -> reqd:H2.Reqd.t -> unit Lwt.t

val server_streaming : f:server_streaming -> reqd:H2.Reqd.t -> unit Lwt.t

val bidirectional_streaming :
  f:bidirectional_streaming -> reqd:H2.Reqd.t -> unit Lwt.t
