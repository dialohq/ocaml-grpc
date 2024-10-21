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
}

type ('net_response, 'headers) resp_not_ok = {
  net_response : 'net_response;
  grpc_status : Grpc.Status.t;
  trailers : 'headers;
}

type ('net_response, 'headers, 'conn_err) common_error =
  [ `Connection_error of 'conn_err
  | `Response_not_ok of ('net_response, 'headers) resp_not_ok ]

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

type ('stream_err, 'headers) streaming_err = {
  stream_error : 'stream_err option;
  write_exn : exn option;
  grpc_status : Grpc.Status.t;
}

type ('a, 'headers, 'stream_err) streaming_result = {
  result : 'a;
  trailers : 'headers;
  err : ('stream_err, 'headers) streaming_err option;
}

module Unary : sig
  type ('net_response, 'headers, 'stream_err) premature_close = {
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    net_response : 'net_response;
    stream_error : 'stream_err option;
  }

  type ('net_response, 'response, 'headers) success = {
    net_response : 'net_response;
    response : 'response;
    trailers : 'headers;
  }

  type ('response, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Premature_close of ('net_response, 'headers, 'stream_err) premature_close
    | `Success of ('net_response, 'response, 'headers) success
    | `Write_error of exn
    | ('net_response, 'headers, 'conn_err) common_error ]

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
  type ('a, 'headers, 'stream_err) stream_err = {
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    result : 'a;
    stream_error : 'stream_err;
    write_exn : exn option;
  }

  type ('a, 'response, 'headers) success = {
    result : 'a;
    response : 'response;
    trailers : 'headers;
    write_exn : exn option;
  }

  type ('a, 'headers) premature_close = {
    result : 'a;
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    write_exn : exn option;
  }

  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response, 'response) result' =
    [ `Premature_close of ('a, 'headers) premature_close
    | `Stream_error of ('a, 'headers, 'stream_err) stream_err
    | `Success of ('a, 'response, 'headers) success
    | ('net_response, 'headers, 'conn_err) common_error ]

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
    [ `Stream_result of ('a, 'headers, 'stream_error) streaming_result
    | `Write_error of ('stream_error, 'headers) streaming_err option * 'headers
    | ('net_response, 'headers, 'conn_err) common_error ]
end

module Bidirectional_streaming : sig
  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response) result' =
    [ `Stream_result of ('a, 'headers, 'stream_err) streaming_result
    | ('net_response, 'headers, 'conn_err) common_error ]
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
