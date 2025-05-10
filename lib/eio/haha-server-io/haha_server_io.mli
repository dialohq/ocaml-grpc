type net_request = {
  request : Haha.Request.t;
  msg_stream :
    Grpc_eio_core.Body_parse.t Grpc_eio_core.Body_parse.consumer option
    Eio.Stream.t;
  handler_resolver :
    (Haha.Types.body_reader
    * Haha.Response.response_writer
    * (Haha.Error.stream_error -> unit))
    Eio.Promise.u;
  connection_error : Haha.Error.connection_error Eio.Promise.t;
  buffer_pool : Grpc_eio_core.Buffer_pool.Bytes_pool.t;
}

include
  Grpc_server_eio.Io.S
    with type Net_request.t = net_request
     and type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit

val connection_handler :
  sw:Eio.Switch.t ->
  ?debug:bool ->
  ?config:Haha.Settings.t ->
  ?grpc_error_handler:(exn -> (string * string) list) ->
  (Net_request.t, request, response) Grpc_server_eio.t ->
  'a Eio.Net.connection_handler
