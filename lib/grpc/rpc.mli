type buffer = string

(** Exploring a separate client/server api that works better with [ocaml-protoc]. *)

module Service_spec : sig
  type t = { package : string list; service_name : string }

  val packaged_service_name : t -> string
end

module Client_rpc : sig
  type ('request, 'response) t = {
    service_spec : Service_spec.t;
    rpc_name : string;
    encode_request : 'request -> buffer;
    decode_response : buffer -> 'response;
  }

  val packaged_service_name : _ t -> string
end

module Server_rpc : sig
  module Service_spec : sig
    type 'a t = None : unit t | Some : Service_spec.t -> Service_spec.t t
  end

  type ('request, 'response, 'service_spec) t = {
    service_spec : 'service_spec Service_spec.t;
    rpc_name : string;
    decode_request : buffer -> 'request;
    encode_response : 'response -> buffer;
  }
end
