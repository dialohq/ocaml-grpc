type t
type data_writer = unit -> Cstruct.t list option
type data_receiver = Cstruct.t option -> unit

val create :
  ?max_streams:int -> sw:Eio.Switch.t -> net:_ Eio.Net.t -> string -> t

val start_request :
  t ->
  headers:Grpc_client.request_headers ->
  data_writer:data_writer ->
  data_receiver:data_receiver ->
  path:string ->
  Status_new.t Eio.Promise.t

val shutdown : t -> unit
