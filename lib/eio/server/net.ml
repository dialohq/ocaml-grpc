type streaming_writer = {
  on_msg : string -> unit;
  close : unit -> unit;
  write_trailers : Grpc_server.trailers -> unit;
}

module type S = sig
  module Request : sig
    type t

    val read_body : t -> string option Eio.Stream.t
    val is_post : t -> bool
    val target : t -> string
    val get_header : t -> string -> string option
  end

  val respond_streaming :
    headers:Grpc_server.headers -> Request.t -> streaming_writer

  val respond_error : Request.t -> Grpc_server.error -> unit
end

type 'request t = (module S with type Request.t = 'request)
