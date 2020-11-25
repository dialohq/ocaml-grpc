(** The type of a Client *)
module type S = sig
  type t
  (** [t] represents a client and the associated services and routing information. *)

  val v : unit -> t
  (** [v ()] creates a new server. *)

  val add_service : name:string -> service:(H2.Reqd.t -> unit) -> t -> t
  (** [add_service ~name ~service t] adds [service] to [t] and ensures that it is routable via [name]. *)

  val make_request :
    t -> service:string -> rpc:string -> request:bytes -> H2.Reqd.t
  (** [make_request t reqd] routes [reqd] to the appropriate service in [t] if available. *)
end
