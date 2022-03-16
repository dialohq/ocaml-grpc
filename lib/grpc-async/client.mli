open! Async

module Rpc : sig
  type 'a handler =
    [ `write ] H2.Body.t -> [ `read ] H2.Body.t Deferred.t -> 'a Deferred.t

  val unary :
    f:([ `Eof | `Ok of string ] Deferred.t -> 'a Deferred.t) ->
    string ->
    'a handler
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
  ?headers:H2.Headers.t ->
  unit ->
  ('a * Grpc.Status.t, Grpc.Status.t) Core._result Deferred.t
(** [call ~service ~rpc ~handler ~do_request ()] calls the rpc endpoint given
      by [service] and [rpc] using the [do_request] function. The [handler] is
      called when this request is set up to send and receive data. *)
