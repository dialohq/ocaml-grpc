module Rpc : sig
  type 'a handler =
    [ `write ] H2.Body.t -> [ `read ] H2.Body.t Lwt.t -> 'a Lwt.t

  val bidirectional_streaming :
    f:((string -> unit) -> Grpc.Buffer.t Lwt_stream.t -> 'a Lwt.t) -> 'a handler
  (** [bidirectional_streaming ~f write read] sets up the sending and receiving
      logic using [write] and [read], then calls [f] with a push function for
      requests and a stream of responses. *)

  val client_streaming :
    f:((string -> unit) -> Grpc.Buffer.t option Lwt.t -> 'a Lwt.t) -> 'a handler
  (** [client_streaming ~f write read] sets up the sending and receiving
      logic using [write] and [read], then calls [f] with a push function for
      requests and promise for the response. *)

  val server_streaming :
    f:(Grpc.Buffer.t Lwt_stream.t -> 'a Lwt.t) -> string -> 'a handler
  (** [server_streaming ~f enc write read] sets up the sending and receiving
      logic using [write] and [read], then sends [enc] and calls [f] with a
      stream of responses. *)

  val unary : f:(Grpc.Buffer.t option Lwt.t -> 'a Lwt.t) -> string -> 'a handler
  (** [unary ~f enc write read] sets up the sending and receiving
      logic using [write] and [read], then sends [enc] and calls [f] with a
      promise for the response. *)
end

type response_handler = H2.Client_connection.response_handler

type do_request =
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  [ `write ] H2.Body.t
(** [do_request] is the type of a function that performs the request *)

val call :
  service:string ->
  rpc:string ->
  ?scheme:string ->
  handler:'a Rpc.handler ->
  do_request:do_request ->
  unit ->
  ('a * Grpc.Status.t, Grpc.Status.t) result Lwt.t
(** [call ~service ~rpc ~handler ~do_request ()] calls the rpc endpoint given
      by [service] and [rpc] using the [do_request] function. The [handler] is
      called when this request is set up to send and receive data. *)
