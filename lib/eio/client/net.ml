type writer = { write : string -> unit; close : unit -> unit }

type ('response, 'headers) reader = {
  response : 'response;
  body_reader : string option Eio.Stream.t;
  trailers : 'headers Eio.Promise.t;
}

module type S = sig
  module Headers : sig
    type t

    val get : t -> string -> string option
  end

  module Response : sig
    type t

    val is_ok : t -> bool
    val headers : t -> Headers.t
  end

  type connection_error

  val send_request :
    headers:Grpc_client.request_headers ->
    string ->
    ( writer
      * ((Response.t, Headers.t) reader, connection_error) result Eio.Promise.t,
      connection_error )
    result
end

type ('headers, 'response, 'connection_error) t =
  (module S
     with type Response.t = 'response
      and type Headers.t = 'headers
      and type connection_error = 'connection_error)
