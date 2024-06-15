type 'request streaming_writer = {
  (* replace dis string *)
  write : 'request -> unit;
  close : unit -> unit;
  write_trailers : Grpc_server.trailers -> unit;
  is_closed : unit -> bool;
}

module type S = sig
  type request

  module Net_request : sig
    type t

    val body : t -> request Seq.t
    val is_post : t -> bool
    val target : t -> string
    val get_header : t -> string -> string option
  end

  type response

  val respond_streaming :
    headers:Grpc_server.headers -> Net_request.t -> response streaming_writer

  val respond_error :
    status_code:int -> headers:(string * string) list -> Net_request.t -> unit
end

type ('net_request, 'request, 'response) t =
  (module S
     with type Net_request.t = 'net_request
      and type request = 'request
      and type response = 'response)
