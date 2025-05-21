type 'c t

type 'context data_receiver_result = {
  action : [ `Continue | `Reset ];
  context : 'context;
}

type 'context data_receiver =
  'context -> Cstruct.t -> 'context data_receiver_result

type 'context data_writer = 'context -> Cstruct.t list option * 'context
type 'context stream_result = { status : Status.t; grpc_context : 'context }

val create :
  ?max_streams:int -> sw:Eio.Switch.t -> net:_ Eio.Net.t -> string -> _ t

val start_request :
  'c t ->
  headers:Legacy_modules.Grpc_client.request_headers ->
  data_writer:'c data_writer ->
  data_receiver:'c data_receiver ->
  path:string ->
  initial_context:'c ->
  'c stream_result Eio.Promise.t

val shutdown : _ t -> unit
