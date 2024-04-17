exception Network_error_todo_remove of H2.Client_connection.error

type connection_error = H2.Client_connection.error

val create_client :
  net:Eio_unix.Net.t ->
  sw:Eio.Switch.t ->
  string ->
  (H2.Headers.t, H2.Response.t, connection_error) Grpc_client_eio.Net.t

module Expert : sig
  val create_with_socket :
    sw:Eio.Switch.t ->
    socket:_ Eio.Net.stream_socket ->
    host:string ->
    scheme:string ->
    (H2.Headers.t, H2.Response.t, connection_error) Grpc_client_eio.Net.t

  val create_with_address :
    net:Eio_unix.Net.t ->
    sw:Eio.Switch.t ->
    scheme:string ->
    host:string ->
    port:int ->
    (H2.Headers.t, H2.Response.t, connection_error) Grpc_client_eio.Net.t
end
