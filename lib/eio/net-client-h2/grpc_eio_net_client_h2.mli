module Unmanaged : sig
  type connection_error = H2.Client_connection.error

  type t = {
    net : (H2.Headers.t, H2.Response.t, connection_error) Grpc_client_eio.Net.t;
    connection_error : unit -> connection_error option;
  }

  val create_client : net:Eio_unix.Net.t -> sw:Eio.Switch.t -> string -> t

  module Expert : sig
    val create_with_socket :
      sw:Eio.Switch.t ->
      socket:_ Eio.Net.stream_socket ->
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
end
