module Rpc : sig
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t Eio.Promise.t -> 'a
  (** [handler] is a function that implements an RPC by sending and receiving
      gRPC messages over a single HTTP/2 stream.

      [write_body] is available immediately; the handler should begin sending
      the request body without waiting for [read_body_p] to resolve.
      [read_body_p] resolves to [H2.Body.Reader.t] once the server sends its
      response HEADERS frame.

      The gRPC over HTTP/2 protocol and RFC 9113 §5.1 place no ordering
      constraint between client DATA frames and server HEADERS; a conforming
      server may withhold response HEADERS until after END_STREAM.  Pipelining
      [write_body] calls before awaiting [read_body_p] is therefore required
      for correct interoperability. *)

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
  ('a * Grpc.Status.t, H2.Status.t) result
(** [call ~service ~rpc ~handler ~do_request ()] calls the rpc endpoint given
        by [service] and [rpc] using the [do_request] function. The [handler] is
        called when this request is set up to send and receive data. *)
