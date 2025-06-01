open Pbrt
open Utils

type imp_writer = (Encoder.t -> unit) option -> unit
type imp_reader = Decoder.t Seq.t
type 'a reading_handler = reader:imp_reader -> 'a
type 'a writing_handler = writer:imp_writer -> 'a
type 'a bi_handler = writer:imp_writer -> reader:imp_reader -> 'a

module Unary : sig
  val call :
    channel:Channel.t ->
    service:string ->
    method_name:string ->
    (Encoder.t -> unit) ->
    (Decoder.t, Status.t) result
end

module ServerStreaming : sig
  module Expert : sig
    val call :
      channel:Channel.t ->
      initial_context:'a ->
      service:string ->
      method_name:string ->
      (Encoder.t -> unit) ->
      'a stream_reader ->
      ('a, Status.t) result
  end

  val call :
    channel:Channel.t ->
    service:string ->
    method_name:string ->
    (Encoder.t -> unit) ->
    'a reading_handler ->
    ('a, Status.t) result
end

module ClientStreaming : sig
  module Expert : sig
    val call :
      channel:Channel.t ->
      initial_context:'a ->
      service:string ->
      method_name:string ->
      'a stream_writer ->
      (Decoder.t * 'a, Status.t) result
  end

  val call :
    channel:Channel.t ->
    service:string ->
    method_name:string ->
    'a writing_handler ->
    (Decoder.t * 'a, Status.t) result
end

module BidirectionalStreaming : sig
  module Expert : sig
    val call :
      channel:Channel.t ->
      initial_context:'a ->
      service:string ->
      method_name:string ->
      'a stream_writer ->
      'a stream_reader ->
      ('a, Status.t) result
  end

  val call :
    channel:Channel.t ->
    service:string ->
    method_name:string ->
    'a bi_handler ->
    ('a, Status.t) result
end
