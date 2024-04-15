include
  Grpc_server_eio.Net.S
    with type Request.t = Eio.Net.Sockaddr.stream * H2.Reqd.t * H2.Request.t

val connection_handler :
  sw:Eio.Switch.t ->
  ?config:H2.Config.t ->
  ?error_handler:
    (Eio.Net.Sockaddr.stream ->
    ?request:H2.Request.t ->
    H2.Server_connection.error ->
    unit) ->
  Request.t Grpc_server_eio.Rpc.handler Grpc_server.t ->
  'a Eio.Net.connection_handler
