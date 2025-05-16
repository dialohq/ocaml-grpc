type 'c t
type 'context data_writer = 'context -> Cstruct.t list option * 'context
type 'context data_receiver = 'context -> Cstruct.t option -> 'context

val create :
  ?max_streams:int -> sw:Eio.Switch.t -> net:_ Eio.Net.t -> string -> _ t

val start_request :
  headers:Grpc_client.request_headers ->
  data_writer:'c data_writer ->
  data_receiver:'c data_receiver ->
  path:string ->
  initial_context:'c ->
  'c t ->
  Status_new.t Eio.Promise.t

val shutdown : _ t -> unit
