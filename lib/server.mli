exception GrpcError of (Status.code * string)

open Haha
open Pbrt
open Utils

type route_getter = service:string -> meth:string -> Reqd.handler_result option

module Unary : sig
  val respond : (Decoder.t -> single_writer) -> Reqd.handler_result
end

module ServerStreaming : sig
  val respond :
    (unit -> 'a) -> (Decoder.t -> 'a stream_writer) -> Reqd.handler_result
end

module ClientStreaming : sig
  val respond :
    (unit -> 'a) ->
    'a stream_reader ->
    ('a -> single_writer) ->
    Reqd.handler_result
end

module BidirectionalStreaming : sig
  val respond :
    (unit -> 'a) ->
    'a stream_reader ->
    'a stream_writer ->
    ('a -> unit) ->
    Reqd.handler_result
end

val connection_handler : route_getter -> _ Eio.Net.connection_handler
