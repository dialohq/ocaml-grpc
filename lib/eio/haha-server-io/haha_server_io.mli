include
  Grpc_server_eio.Io.S
    with type Net_request.t =
      Haha.Request.t
      * Grpc_eio_core.Body_parse.t Grpc_eio_core.Body_parse.consumer option
        Eio.Stream.t
      * (Haha.Types.body_reader * Haha.Response.response_writer) Eio.Promise.u
      * Haha.Error.connection_error Eio.Promise.t
     and type request = Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer
     and type response = Pbrt.Encoder.t -> unit

val connection_handler :
  sw:Eio.Switch.t ->
  ?debug:bool ->
  ?config:Haha.Settings.t ->
  ?h2_error_handler:(Haha.Error.t -> unit) ->
  ?grpc_error_handler:(exn -> (string * string) list) ->
  (Net_request.t, request, response) Grpc_server_eio.t ->
  'a Eio.Net.connection_handler
