type t

val v : unit -> t

val add_rpc : name:string -> rpc:Rpc.t -> t -> t

val handle_request : t -> H2.Reqd.t -> unit
