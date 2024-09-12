type stream_error = [ H2.Client_connection.error | `Unexpected_eof ]

type t =
  ( H2.Headers.t,
    H2.Response.t,
    Pbrt.Encoder.t -> unit,
    Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
    stream_error,
    H2.Client_connection.error )
  Grpc_client_eio.Io.t

module Expert : sig
  val create_with_socket :
    sw:Eio.Switch.t ->
    socket:[> [> `Generic ] Eio.Net.stream_socket_ty ] Eio_unix.source ->
    host:string ->
    scheme:string ->
    t

  val create_with_address :
    net:Eio_unix.Net.t ->
    sw:Eio.Switch.t ->
    scheme:string ->
    host:string ->
    port:int ->
    t
end

val create_client : net:Eio_unix.Net.t -> sw:Eio.Switch.t -> string -> t