(** The type of a Server *)
module type S = sig
  type t
  (** [t] represents a server and its associated services and routing information. *)

  val v : unit -> t
  (** [v ()] creates a new server. *)

  val add_service : name:string -> service:(H2.Reqd.t -> unit) -> t -> t
  (** [add_service ~name ~service t] adds [service] to [t] and ensures that it is routable via [name]. *)

  val handle_request : t -> H2.Reqd.t -> unit
  (** [handle_request t reqd] routes [reqd] to the appropriate service in [t] if available. *)
end
