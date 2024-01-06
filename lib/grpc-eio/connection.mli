module Typed : sig
  val grpc_recv_streaming :
    decode:(string -> 'a) -> H2.Body.Reader.t -> 'a Seq.writer -> unit

  val grpc_send_streaming_client :
    encode:('a -> string) -> H2.Body.Writer.t -> 'a Seq.reader -> unit

  val grpc_send_streaming :
    encode:('a -> string) ->
    H2.Reqd.t ->
    'a Seq.reader ->
    Grpc.Status.t Eio.Promise.t ->
    unit
end

module Untyped : sig
  val grpc_recv_streaming : H2.Body.Reader.t -> string Seq.writer -> unit

  val grpc_send_streaming :
    H2.Reqd.t -> string Seq.reader -> Grpc.Status.t Eio.Promise.t -> unit
end
