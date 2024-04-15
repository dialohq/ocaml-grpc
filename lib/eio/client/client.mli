type ('response, 'conn_error) connection = {
  writer : Net.writer;
  recv : ('response * Grpc_core_eio.Stream.t, 'conn_error) result Eio.Promise.t;
  grpc_status : Grpc.Status.t Eio.Promise.t;
}

type ('decoding_error, 'connection_error, 'response) error =
  [ `Decoding of 'decoding_error
  | `Rpc of 'response * Grpc.Status.t
  | `Connection of 'connection_error ]

type ('ok, 'decoding_error, 'connection_error, 'net_response) rpc_result =
  ('ok, ('decoding_error, 'connection_error, 'net_response) error) result

val call :
  sw:Eio.Switch.t ->
  net:('headers, 'response, 'connection_error) Net.t ->
  service:string ->
  method_name:string ->
  headers:Grpc_client.request_headers ->
  unit ->
  (('response, 'connection_error) connection, 'connection_error) result

val unary :
  sw:Eio.Switch.t ->
  net:('headers, 'net_response, 'conn_error) Net.t ->
  service:string ->
  method_name:string ->
  decode:(string -> ('response, 'decoding_error) result) ->
  encode:('request -> string) ->
  headers:Grpc_client.request_headers ->
  'request ->
  ('response, 'decoding_error, 'conn_error, 'net_response) rpc_result

val client_streaming :
  sw:Eio.Switch.t ->
  net:('headers, 'net_response, 'conn_error) Net.t ->
  service:string ->
  method_name:string ->
  decode:(string -> ('response, 'decoding_error) result) ->
  encode:('request -> string) ->
  headers:Grpc_client.request_headers ->
  (write:('request -> unit) -> unit) ->
  ('response, 'decoding_error, 'conn_error, 'net_response) rpc_result

val server_streaming :
  sw:Eio.Switch.t ->
  net:('headers, 'net_response, 'conn_error) Net.t ->
  service:string ->
  method_name:string ->
  decode:(string -> ('response, 'decoding_error) result) ->
  encode:('request -> string) ->
  headers:Grpc_client.request_headers ->
  'request ->
  (read:(unit -> 'response option) -> unit) ->
  (unit, 'decoding_error, 'conn_error, 'net_response) rpc_result

type 'a writer = { write : 'a -> unit; close : unit -> unit }

val bidirectional_streaming :
  sw:Eio.Switch.t ->
  net:('headers, 'net_response, 'conn_error) Net.t ->
  service:string ->
  method_name:string ->
  decode:(string -> ('response, 'decoding_error) result) ->
  encode:('request -> string) ->
  headers:Grpc_client.request_headers ->
  (writer:'request writer -> take:(unit -> 'response option) -> unit) ->
  (unit, 'decoding_error, 'conn_error, 'net_response) rpc_result
