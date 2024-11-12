type 'request writer = {
  write : 'request -> unit;
  (* Returns true if the write was successful, false if the stream is in error state. Throws if the stream was closed. *)
  close : unit -> unit;
}

type ('net_response, 'response, 'headers, 'err) reader = {
  response : 'net_response;
  trailers : 'headers Eio.Promise.t;
  next : ('response, 'err) Grpc_eio_core.Recv_seq.t;
}

type ('net_response,
       'response,
       'headers,
       'stream_err,
       'conn_err)
     reader_or_error =
  (('net_response, 'response, 'headers, 'stream_err) reader, 'conn_err) result

module type S = sig
  module Headers : sig
    type t

    val get : t -> string -> string option
  end

  module Net_response : sig
    type t

    val is_ok : t -> bool
    val headers : t -> Headers.t
  end

  type request
  type response
  type connection_error
  type stream_error

  type client_error =
    | Unary of
        ( response,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t )
        Rpc_error.Unary.error'
    | Client_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t,
          response )
        Rpc_error.Client_streaming.error'
        -> client_error
    | Server_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          Net_response.t,
          connection_error )
        Rpc_error.Server_streaming.error'
        -> client_error
    | Bidirectional_streaming :
        ( 'a,
          Headers.t,
          stream_error,
          connection_error,
          Net_response.t )
        Rpc_error.Bidirectional_streaming.error'
        -> client_error

  exception Grpc_client_error of client_error

  val raise_client_error : client_error -> 'exn

  val send_request :
    headers:Grpc_client.request_headers ->
    string ->
    request writer
    * ( Net_response.t,
        response,
        Headers.t,
        stream_error,
        connection_error )
      reader_or_error
      Eio.Promise.t
    * connection_error Eio.Promise.t
end

type ('headers,
       'net_response,
       'request,
       'response,
       'stream_error,
       'connection_error)
     t =
  (module S
     with type Net_response.t = 'net_response
      and type Headers.t = 'headers
      and type connection_error = 'connection_error
      and type stream_error = 'stream_error
      and type request = 'request
      and type response = 'response)
