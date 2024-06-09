include
  Grpc_server_eio.Io.S
    with type Net_request.t = Eio.Net.Sockaddr.stream * H2.Reqd.t * H2.Request.t
     and type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit

val connection_handler :
  sw:Eio.Switch.t ->
  ?config:H2.Config.t ->
  ?h2_error_handler:
    (Eio.Net.Sockaddr.stream ->
    ?request:H2.Request.t ->
    H2.Server_connection.error ->
    unit) ->
  ?grpc_error_handler:(exn -> (string * string) list) ->
  (Net_request.t, request, response) Grpc_server_eio.t ->
  'a Eio.Net.connection_handler
