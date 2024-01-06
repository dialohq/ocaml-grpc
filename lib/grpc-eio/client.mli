type response_handler = H2.Client_connection.response_handler

type do_request =
  ?flush_headers_immediately:bool ->
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  H2.Body.Writer.t
(** [do_request] is the type of a function that performs the request *)

(** {1 Typed API} *)

module Typed_rpc : sig
  (** This is an experimental API to call RPC from the client side. Compared to
      {Rpc}, this interface will:

      - handle the coding/decoding of messages for you under the hood;
      - use the service and RPC names provided by the rpc specification to
        call the services with their expected names. *)

  type ('request, 'request_mode, 'response, 'response_mode, 'a) handler

  (** The next functions are meant to be used by the client to handle
     call to RPCs. *)

  val bidirectional_streaming :
    f:('request Seq.writer -> 'response Seq.t -> 'a) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.stream,
      'a )
    handler

  val client_streaming :
    f:('request Seq.writer -> 'response option Eio.Promise.t -> 'a) ->
    ( 'request,
      Grpc.Rpc.Value_mode.stream,
      'response,
      Grpc.Rpc.Value_mode.unary,
      'a )
    handler

  val server_streaming :
    f:('response Seq.t -> 'a) ->
    'request ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.stream,
      'a )
    handler

  val unary :
    f:('response option -> 'a) ->
    'request ->
    ( 'request,
      Grpc.Rpc.Value_mode.unary,
      'response,
      Grpc.Rpc.Value_mode.unary,
      'a )
    handler

  val call :
    ('request, 'request_mode, 'response, 'response_mode) Grpc.Rpc.Client_rpc.t ->
    ?scheme:string ->
    handler:('request, 'request_mode, 'response, 'response_mode, 'a) handler ->
    do_request:do_request ->
    ?headers:H2.Headers.t ->
    unit ->
    ('a * Grpc.Status.t, H2.Status.t) result
  (** The rpc specification must be provided as it is used to handle
      coding/decoding of messages as well as allows referring to the service
      and RPC names specified in the [.proto] file. *)
end

(** {1 Untyped API} *)

module Rpc : sig
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t -> 'a

  val bidirectional_streaming :
    f:(string Seq.writer -> string Seq.t -> 'a) -> 'a handler
  (** [bidirectional_streaming ~f write read] sets up the sending and receiving
        logic using [write] and [read], then calls [f] with a push function for
        requests and a stream of responses. *)

  val client_streaming :
    f:(string Seq.writer -> string option Eio.Promise.t -> 'a) -> 'a handler
  (** [client_streaming ~f write read] sets up the sending and receiving
        logic using [write] and [read], then calls [f] with a push function for
        requests and promise for the response. *)

  val server_streaming : f:(string Seq.t -> 'a) -> string -> 'a handler
  (** [server_streaming ~f enc write read] sets up the sending and receiving
        logic using [write] and [read], then sends [enc] and calls [f] with a
        stream of responses. *)

  val unary : f:(string option -> 'a) -> string -> 'a handler
  (** [unary ~f enc write read] sets up the sending and receiving
        logic using [write] and [read], then sends [enc] and calls [f] with a
        promise for the response. *)

  val call :
    service:string ->
    rpc:string ->
    ?scheme:string ->
    handler:'a handler ->
    do_request:do_request ->
    ?headers:H2.Headers.t ->
    unit ->
    ('a * Grpc.Status.t, H2.Status.t) result
  (** [call ~service ~rpc ~handler ~do_request ()] calls the rpc endpoint given
        by [service] and [rpc] using the [do_request] function. The [handler] is
        called when this request is set up to send and receive data. *)
end

val call :
  service:string ->
  rpc:string ->
  ?scheme:string ->
  handler:'a Rpc.handler ->
  do_request:do_request ->
  ?headers:H2.Headers.t ->
  unit ->
  ('a * Grpc.Status.t, H2.Status.t) result
(** [@@deprecated "This function was renamed [Grpc_eio.Client.Rpc.call]."] *)
