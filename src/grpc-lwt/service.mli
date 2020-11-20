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

type t

val v : string -> t

val add_rpc : name:string -> rpc:rpc -> t -> t

val to_module : t -> (module Grpc.Service.S)
