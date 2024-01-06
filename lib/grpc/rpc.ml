type buffer = string

module Value_mode = struct
  type unary
  type stream
  type _ t = Unary : unary t | Stream : stream t
end

module Service_spec = struct
  type t = { package : string list; service_name : string }

  let packaged_service_name t =
    String.concat "." (t.package @ [ t.service_name ])
end

module Handlers = struct
  type ('a, 'b) t =
    | Handlers of { handlers : 'a list }
    | With_service_spec of { handlers : 'b list; service_spec : Service_spec.t }
    | ( :: ) of ('a, 'b) t * ('a, 'b) t list
end

module Client_rpc = struct
  type ('request, 'request_mode, 'response, 'response_mode) t = {
    service_spec : Service_spec.t;
    rpc_name : string;
    encode_request : 'request -> buffer;
    decode_response : buffer -> 'response;
    request_mode : 'request_mode Value_mode.t;
    response_mode : 'response_mode Value_mode.t;
  }
end

module Server_rpc = struct
  module Service_spec = struct
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
