(** Creating typed specification for RPCs.

    This module provides the functionality to create typed specifications for
    RPCs. It defines abstractions for both client and server sides to utilize
    the typed interfaces of Grpc. These abstractions can be constructed
    directly by advanced users or via helper libraries for commonly used
    protoc providers supported by Grpc. *)

module Value_mode : sig
  (** A type used to differentiate between unary and stream values.

      Grpc supports the definition of RPCs that either take and return a single
      value or a stream of values. The table below illustrates the four types of
      RPCs that can be defined, distinguished by the {!type:Value_mode.t} of
      their [request_mode] and [response_mode] fields.

      {t
        | request_mode | response_mode | rpc kind                |
        | :----------: | :------------:|:-----------------------:|
        | Unary        | Unary         | unary                   |
        | Unary        | Stream        | server_streaming        |
        | Stream       | Unary         | client_streaming        |
        | Stream       | Stream        | bidirectional_streaming |
      } *)

  type unary
  type stream
  type _ t = Unary : unary t | Stream : stream t
end

module Service_spec : sig
  (** The complete name used to identify a service. *)

  type t = { package : string list; service_name : string }
  (** Services can be qualified by a list of {!field:package} names in addition
      to their {!field:service_name}. Values of this type are typically
      auto-generated from the service interfaces defined in *.proto files. *)

  val packaged_service_name : t -> string
  (** This function constructs a canonical service name that acts as a key to
      identify and retrieve the correct service at runtime. The convention is
      to concatenate the package and service names, separated by a dot. *)
end

type buffer = string
(** The {!type:buffer} type represents the messages exchanged by the low-level
    transport layer of Grpc. The typed specification includes transformation
    functions to convert to and from this wire encoding. Depending on the
    specification's construction, this string may represent messages in JSON
    or Protobuf format. *)

(** {1 Client side} *)

module Client_rpc : sig
  (** RPC specification used by clients when calling gRPCs. *)

  type ('request, 'request_mode, 'response, 'response_mode) t = {
    service_spec : Service_spec.t;
    rpc_name : string;
    encode_request : 'request -> buffer;
    decode_response : buffer -> 'response;
    request_mode : 'request_mode Value_mode.t;
    response_mode : 'response_mode Value_mode.t;
  }
end

(** {1 Server side} *)

module Server_rpc : sig
  (** RPC specification used by server when implementing gRPCs. *)

  module Service_spec : sig
    (** This type indicates whether a {!Service_spec.t} is available in the
        server-side specification.

        Grpc supports several protoc providers (ocaml-protoc & ocaml-protoc-plugin),
        which differ in the information available in their server-side handlers.

        {t
          | protoc library      | service_spec in handler |
          | :-----------------: | :----------------------:|
          | ocaml-protoc        | No |
          | ocaml-protoc-plugin | Yes |
         } *)
    type 'a t = None : unit t | Some : Service_spec.t -> Service_spec.t t
  end

  type ('request, 'request_mode, 'response, 'response_mode, 'service_spec) t = {
    service_spec : 'service_spec Service_spec.t;
    rpc_name : string;
    decode_request : buffer -> 'request;
    encode_response : 'response -> buffer;
    request_mode : 'request_mode Value_mode.t;
    response_mode : 'response_mode Value_mode.t;
  }
end

module Handlers : sig
  (** This type helps distinguish between server handlers that do or do not
      contain the specification of the service they implement. The type is
      parameterized as it is shared by libraries that depend on different
      concurrency libraries, causing the actual type of handlers to vary. *)
  type ('a, 'b) t =
    | Handlers of { handlers : 'a list }
        (** This representation is used when each handler contains a reference
            to the service spec, such as when they are built with
            [ocaml_protoc_plugin]. *)
    | With_service_spec of { handlers : 'b list; service_spec : Service_spec.t }
        (** If the service spec is not represented by each handler, it must be
            passed along with all handlers that implement an RPC for that
            service. *)
    | ( :: ) of ('a, 'b) t * ('a, 'b) t list
        (** This constructor allows multiple services' handlers to be
            implemented on the same server, supplying them grouped using list
            syntax. *)
end
