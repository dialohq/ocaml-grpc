module type S = sig
  val name : string

  val handle_rpc : H2.Reqd.t -> unit
end
