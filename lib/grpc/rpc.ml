type buffer = string

module Service_spec = struct
  type t = { package : string list; service_name : string }

  let packaged_service_name t =
    (match t.package with _ :: _ as p -> String.concat "." p | [] -> "")
    ^ t.service_name
end

module Client_rpc = struct
  type ('request, 'response) t = {
    service_spec : Service_spec.t;
    rpc_name : string;
    encode_request : 'request -> buffer;
    decode_response : buffer -> 'response;
  }

  let packaged_service_name t =
    Service_spec.packaged_service_name t.service_spec
end

module Server_rpc = struct
  module Service_spec = struct
    type 'a t = None : unit t | Some : Service_spec.t -> Service_spec.t t
  end

  type ('request, 'response, 'service_spec) t = {
    service_spec : 'service_spec Service_spec.t;
    rpc_name : string;
    decode_request : buffer -> 'request;
    encode_response : 'response -> buffer;
  }
end
