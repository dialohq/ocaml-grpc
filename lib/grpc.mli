module Server = Server

module Channel : sig
  type t = Channel.t

  val create :
    ?max_streams:int -> sw:Eio.Switch.t -> net:_ Eio.Net.t -> string -> t

  val shutdown : t -> unit
end

module Client = Client
module Status = Status
