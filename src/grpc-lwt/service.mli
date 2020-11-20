type t
(** [t] represents a gRPC service with potentially multiple rpcs and the information needed to route to them. *)

val v : unit -> t
(** [v ()] creates a new service *)

val add_rpc : name:string -> rpc:Rpc.t -> t -> t
(** [add_rpc ~name ~rpc t] adds [rpc] to [t] and ensures that [t] can route to it with [name]. *)

val handle_request : t -> H2.Reqd.t -> unit
(** [handle_request t reqd] handles routing [reqd] to the correct rpc if available in [t]. *)
