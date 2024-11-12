type ('net_response, 'response, 'stream_err, 'headers) recv = {
  net_response : 'net_response;
  recv_seq : ('response, 'stream_err) Grpc_eio_core.Recv_seq.t;
  trailers : 'headers Eio.Promise.t;
}

type 'request writer = { write : 'request -> bool; close : unit -> unit }

type ('net_response,
       'headers,
       'request,
       'response,
       'conn_error,
       'stream_error)
     connection = {
  writer : 'request writer;
  recv :
    ( ('net_response, 'response, 'stream_error, 'headers) recv,
      'conn_error )
    result
    Eio.Promise.t;
  grpc_status : Grpc.Status.t Eio.Promise.t;
  write_exn : exn option ref;
  conn_err : 'conn_error Eio.Promise.t;
}

val call :
  sw:Eio.Switch.t ->
  io:
    ( 'headers,
      'net_response,
      'request,
      'response,
      'stream_error,
      'conn_error )
    Io.t ->
  service:string ->
  method_name:string ->
  headers:Grpc_client.request_headers ->
  unit ->
  ( ( 'net_response,
      'headers,
      'request,
      'response,
      'conn_error,
      'stream_error )
    connection,
    'conn_error )
  result

type ('a, 'headers) streaming_result_success = {
  result : 'a;
  trailers : 'headers;
}

module Unary : sig
  type ('net_response, 'response, 'headers) success = {
    net_response : 'net_response;
    response : 'response;
    trailers : 'headers;
  }

  type ('response, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Success of ('net_response, 'response, 'headers) success
    | ( 'response,
        'headers,
        'stream_err,
        'conn_err,
        'net_response )
      Rpc_error.Unary.error' ]

  val call :
    sw:Eio.Switch.t ->
    io:
      ( 'headers,
        'net_response,
        'request,
        'response,
        'stream_error,
        'conn_error )
      Io.t ->
    service:string ->
    method_name:string ->
    headers:Grpc_client.request_headers ->
    'request ->
    ('response, 'headers, 'stream_error, 'conn_error, 'net_response) result'
end

module Client_streaming : sig
  type ('a, 'response, 'headers) success = {
    result : 'a;
    response : 'response;
    trailers : 'headers;
    write_exn : exn option;
  }

  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response, 'response) result' =
    [ `Success of ('a, 'response, 'headers) success
    | ( 'a,
        'headers,
        'stream_err,
        'conn_err,
        'net_response,
        'response )
      Rpc_error.Client_streaming.error' ]

  val call :
    sw:Eio.Switch.t ->
    io:
      ( 'headers,
        'net_response,
        'request,
        'response,
        'stream_error,
        'conn_error )
      Io.t ->
    service:string ->
    method_name:string ->
    headers:Grpc_client.request_headers ->
    ('net_response -> writer:'request writer -> 'a) ->
    ('a, 'headers, 'stream_error, 'conn_error, 'net_response, 'response) result'
end

module Server_streaming : sig
  type ('a, 'headers, 'stream_error, 'net_response, 'conn_err) result' =
    [ `Stream_result_success of ('a, 'headers) streaming_result_success
    | ( 'a,
        'headers,
        'stream_error,
        'net_response,
        'conn_err )
      Rpc_error.Server_streaming.error' ]

  val call :
    sw:Eio.Switch.t ->
    io:
      ( 'headers,
        'net_response,
        'request,
        'response,
        'stream_error,
        'conn_err )
      Io.t ->
    service:string ->
    method_name:string ->
    headers:Grpc_client.request_headers ->
    'request ->
    ((unit -> ('net_response, 'conn_err) result) ->
    read:(unit -> 'response Seq.node) ->
    'a) ->
    ('a, 'headers, 'stream_error, 'net_response, 'conn_err) result'
end

module Bidirectional_streaming : sig
  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Stream_result_success of ('a, 'headers) streaming_result_success
    | ( 'a,
        'headers,
        'stream_err,
        'conn_err,
        'net_response )
      Rpc_error.Bidirectional_streaming.error' ]
  (*
  val call :
    sw:Eio.Switch.t ->
    io:
      ( 'headers,
        'net_response,
        'request,
        'response,
        'stream_error,
        'conn_error )
      Io.t ->
    service:string ->
    method_name:string ->
    headers:Grpc_client.request_headers ->
    ?init_requests:'request Seq.t ->
    ('net_response ->
    'request Seq.t ->
    writer:'request writer ->
    read:(unit -> 'response Seq.node) ->
    'a) ->
    ('a, 'headers, 'stream_error, 'conn_error, 'net_response) result'
    *)

  val call :
    sw:Eio.Switch.t ->
    io:
      ( 'headers,
        'net_response,
        'request,
        'response,
        'stream_error,
        'conn_error )
      Io.t ->
    service:string ->
    method_name:string ->
    headers:Grpc_client.request_headers ->
    ((unit -> ('net_response, 'conn_error) result) ->
    writer:'request writer ->
    read:(unit -> 'response Seq.node) ->
    'a) ->
    ('a, 'headers, 'stream_error, 'conn_error, 'net_response) result'
end
