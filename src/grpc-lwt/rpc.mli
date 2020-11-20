val unary :
  f:(Pbrt.Decoder.t -> (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t) ->
  reqd:H2.Reqd.t ->
  unit Lwt.t

val client_streaming :
  f:
    (Pbrt.Decoder.t Lwt_stream.t ->
    (Grpc.Status.t * Pbrt.Encoder.t option) Lwt.t) ->
  reqd:H2.Reqd.t ->
  unit Lwt.t

val server_streaming :
  f:(Pbrt.Decoder.t -> (Pbrt.Encoder.t -> unit) -> Grpc.Status.t Lwt.t) ->
  reqd:H2.Reqd.t ->
  unit Lwt.t

val bidirectional_streaming :
  f:
    (Pbrt.Decoder.t Lwt_stream.t ->
    (Pbrt.Encoder.t -> unit) ->
    Grpc.Status.t Lwt.t) ->
  reqd:H2.Reqd.t ->
  unit Lwt.t
