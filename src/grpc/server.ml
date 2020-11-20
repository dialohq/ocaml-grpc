module type S = sig
  type t

  val v : unit -> t

  val add_service : service:(module Service.S) -> t -> t

  val add_services : services:(module Service.S) list -> t -> t

  val handle_request : t -> reqd:H2.Reqd.t -> unit
end
