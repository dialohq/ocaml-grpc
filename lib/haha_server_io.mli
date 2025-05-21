type 'context net_request = {
  request : Haha.Reqd.t;
  msg_stream : Body_parse.t Body_parse.consumer option Eio.Stream.t;
  handler_resolver : 'context Haha.Reqd.handler Eio.Promise.u;
  connection_error : Haha.Error.connection_error Eio.Promise.t;
  buffer_pool : Buffer_pool.Bytes_pool.t;
}

include
  Legacy_modules.Grpc_server_eio.Io.S
    with type Net_request.t = unit net_request
     and type request = Pbrt.Decoder.t Legacy_modules.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit

val connection_handler :
  sw:Eio.Switch.t ->
  ?debug:bool ->
  ?config:Haha.Settings.t ->
  ?grpc_error_handler:(exn -> (string * string) list) ->
  (Net_request.t, request, response) Legacy_modules.Grpc_server_eio.t ->
  'a Eio.Net.connection_handler
