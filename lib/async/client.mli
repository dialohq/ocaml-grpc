open! Async

module Rpc : sig
  type 'a handler =
    H2.Body.Writer.t -> H2.Body.Reader.t Deferred.t -> 'a Deferred.t

  val bidirectional_streaming :
    handler:(string Pipe.Writer.t -> string Pipe.Reader.t -> 'a Deferred.t) ->
    'a handler
  (** [bidirectional_streaming ~handler write read] sets up the sending and
      receiving logic using [write] and [read], then calls [handler] with a
      writer pipe and a reader pipe, for sending and receiving payloads to and
      from the server.

      The stream is closed when the deferred returned by the handler becomes
      determined. *)

  val client_streaming :
    handler:(string Pipe.Writer.t -> string option Deferred.t -> 'a Deferred.t) ->
    'a handler
  (** [client_streaming ~handler write read] sets up the sending and receiving
      logic using [write] and [read], then calls [handler] with a writer pipe to
      send payloads to the server.

      The stream is closed when the deferred returned by the handler becomes
      determined. *)

  val server_streaming :
    handler:(string Pipe.Reader.t -> 'a Deferred.t) ->
    encoded_request:string ->
    'a handler
  (** [server_streaming ~handler encoded_request write read] sets up the sending
      and receiving logic using [write] and [read], then sends [encoded_request]
      and calls [handler] with a pipe of responses.

      The stream is closed when the deferred returned by the handler becomes
      determined. *)

  val unary :
    handler:(string option -> 'a Deferred.t) ->
    encoded_request:string ->
    'a handler
  (** [unary ~handler ~encoded_request] sends the encoded request to the server
      . When the response is received, the handler is called with an option
      response. The response is is None if the server sent an empty response. *)
end

type response_handler = H2.Client_connection.response_handler

type do_request =
  ?flush_headers_immediately:bool ->
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  H2.Body.Writer.t
(** [do_request] is the type of a function that performs the request *)

val call :
  service:string ->
  rpc:string ->
  ?scheme:string ->
  handler:'a Rpc.handler ->
  do_request:do_request ->
  ?headers:H2.Headers.t ->
  unit ->
  ('a * Grpc.Status.t, H2.Status.t) Core._result Deferred.t
(** [call ~service ~rpc ~handler ~do_request ()] calls the rpc endpoint given by
    [service] and [rpc] using the [do_request] function. The [handler] is called
    when this request is set up to send and receive data. *)
