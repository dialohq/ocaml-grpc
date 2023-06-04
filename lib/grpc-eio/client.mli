module Rpc : sig
  type 'a handler = H2.Body.Writer.t -> H2.Body.Reader.t -> 'a

  val bidirectional_streaming :
    f:(send:(string -> unit) -> close:(unit -> unit) -> Stream.t -> 'a) ->
    'a handler
  (** [bidirectional_streaming ~f write read] sets up the sending and receiving
        logic using [write] and [read], then calls [f] with a push function for
        requests and a stream of responses. *)

  val client_streaming :
    f:
      (send:(string -> unit) ->
      close:(unit -> unit) ->
      string option Eio.Promise.t ->
      'a) ->
    'a handler
  (** [client_streaming ~f write read] sets up the sending and receiving
        logic using [write] and [read], then calls [f] with a push function for
        requests and promise for the response. *)

  val server_streaming : f:(Stream.t -> 'a) -> string -> 'a handler
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
