module type S = sig
  type t

  val v : unit -> t

  val add_service : name:string -> service:(H2.Reqd.t -> unit) -> t -> t

  val handle_request : t -> reqd:H2.Reqd.t -> unit
end
