module type S = sig
  type t

  val v : unit -> t

  val add_service : (module Service.S) -> t -> unit

  val add_services : (module Service.S) list -> t -> unit

  val handle_request : t -> H2.Reqd.t -> unit
end
