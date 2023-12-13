let encode (type a) (encode : a -> Pbrt.Encoder.t -> unit) (a : a) =
  let encoder = Pbrt.Encoder.create () in
  encode a encoder;
  Pbrt.Encoder.to_string encoder

let decode (type a) (decode : Pbrt.Decoder.t -> a) buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  decode decoder

let client_rpc (type request response)
    (rpc : (request, _, response, _) Pbrt_services.Client.rpc) =
  {
    Grpc.Rpc.Client_rpc.service_spec =
      { package = rpc.package; service_name = rpc.service_name };
    rpc_name = rpc.rpc_name;
    encode_request = encode rpc.encode_pb_req;
    decode_response = decode rpc.decode_pb_res;
  }

let server_rpc (type request response)
    (rpc : (request, _, response, _) Pbrt_services.Server.rpc) =
  {
    Grpc.Rpc.Server_rpc.service_spec = None;
    rpc_name = rpc.name;
    decode_request = decode rpc.decode_pb_req;
    encode_response = encode rpc.encode_pb_res;
  }
