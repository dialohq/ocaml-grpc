let encode (type a) (encode : a -> Pbrt.Encoder.t -> unit) (a : a) =
  let encoder = Pbrt.Encoder.create () in
  encode a encoder;
  Pbrt.Encoder.to_string encoder

let decode (type a) (decode : Pbrt.Decoder.t -> a) buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  decode decoder

let client_service_spec (rpc : _ Pbrt_services.Client.rpc) =
  {
    Grpc.Rpc.Service_spec.package = rpc.package;
    service_name = rpc.service_name;
  }

module Client_rpc = struct
  let make (type request response)
      (rpc : (request, _, response, _) Pbrt_services.Client.rpc) ~request_mode
      ~response_mode =
    {
      Grpc.Rpc.Client_rpc.service_spec = client_service_spec rpc;
      rpc_name = rpc.rpc_name;
      encode_request = encode rpc.encode_pb_req;
      decode_response = decode rpc.decode_pb_res;
      request_mode;
      response_mode;
    }

  let unary rpc = make rpc ~request_mode:Unary ~response_mode:Unary
  let client_streaming rpc = make rpc ~request_mode:Stream ~response_mode:Unary
  let server_streaming rpc = make rpc ~request_mode:Unary ~response_mode:Stream

  let bidirectional_streaming rpc =
    make rpc ~request_mode:Stream ~response_mode:Stream
end

module Server_rpc = struct
  let make (type request response)
      (rpc : (request, _, response, _) Pbrt_services.Server.rpc) ~request_mode
      ~response_mode =
    {
      Grpc.Rpc.Server_rpc.service_spec = None;
      rpc_name = rpc.name;
      decode_request = decode rpc.decode_pb_req;
      encode_response = encode rpc.encode_pb_res;
      request_mode;
      response_mode;
    }

  let unary rpc = make rpc ~request_mode:Unary ~response_mode:Unary
  let client_streaming rpc = make rpc ~request_mode:Stream ~response_mode:Unary
  let server_streaming rpc = make rpc ~request_mode:Unary ~response_mode:Stream

  let bidirectional_streaming rpc =
    make rpc ~request_mode:Stream ~response_mode:Stream
end

let server_service_spec
    { Pbrt_services.Server.package; service_name; handlers = _ } =
  { Grpc.Rpc.Service_spec.package; service_name }

let handlers
    ({ Pbrt_services.Server.package = _; service_name = _; handlers } as server)
    =
  Grpc.Rpc.Handlers.With_service_spec
    { service_spec = server_service_spec server; handlers }
