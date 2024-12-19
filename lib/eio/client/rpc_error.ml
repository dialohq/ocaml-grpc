type ('net_response, 'headers) resp_not_ok = {
  net_response : 'net_response;
  grpc_status : Grpc.Status.t;
  trailers : 'headers;
}

type ('net_response, 'headers, 'conn_err) common_error =
  [ `Connection_error of 'conn_err
  | `Response_not_ok of ('net_response, 'headers) resp_not_ok ]

module Unary = struct
  type ('net_response, 'headers, 'stream_err) premature_close = {
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    net_response : 'net_response;
    stream_error : 'stream_err option;
  }

  type ('response, 'headers, 'stream_err, 'conn_err, 'net_response) error' =
    [ `Premature_close of ('net_response, 'headers, 'stream_err) premature_close
    | `Write_error of exn
    | ('net_response, 'headers, 'conn_err) common_error ]
end

module Client_streaming = struct
  type ('a, 'headers) premature_close = {
    result : 'a;
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    write_exn : exn option;
  }

  type ('a, 'headers, 'stream_err) stream_err = {
    trailers : 'headers;
    grpc_status : Grpc.Status.t;
    result : 'a;
    stream_error : 'stream_err;
    write_exn : exn option;
  }

  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response, 'response) error' =
    [ `Premature_close of ('a, 'headers) premature_close
    | `Stream_error of ('a, 'headers, 'stream_err) stream_err
    | ('net_response, 'headers, 'conn_err) common_error ]
end

type ('stream_err, 'headers) streaming_err = {
  stream_error : 'stream_err option;
  write_exn : exn option;
  grpc_status : Grpc.Status.t;
}

type ('a, 'headers, 'stream_err) streaming_result_err = {
  result : 'a;
  trailers : 'headers;
  err : ('stream_err, 'headers) streaming_err;
}

module Server_streaming = struct
  type ('a, 'headers, 'stream_error, 'net_response, 'conn_err) error' =
    [ `Stream_result_error of ('a, 'headers, 'stream_error) streaming_result_err
    | `Write_error of ('stream_error, 'headers) streaming_err option * 'headers
    | ('net_response, 'headers, 'conn_err) common_error ]
end

module Bidirectional_streaming = struct
  type ('a, 'headers, 'stream_err, 'conn_err, 'net_response) error' =
    [ `Stream_result_error of ('a, 'headers, 'stream_err) streaming_result_err
    | ('net_response, 'headers, 'conn_err) common_error ]
end
