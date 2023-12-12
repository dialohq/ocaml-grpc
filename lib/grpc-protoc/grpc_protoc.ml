let encode (type a) (encode : a -> Pbrt.Encoder.t -> unit) (a : a) =
  let encoder = Pbrt.Encoder.create () in
  encode a encoder;
  Pbrt.Encoder.to_string encoder

let decode (type a) (decode : Pbrt.Decoder.t -> a) buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  decode decoder

let rpc (type request response)
    ~(client : (request, _, response, _) Pbrt_services.Client.rpc)
    ~(server :
       ((request, 'c, response, 'd) Pbrt_services.Server.rpc ->
       (request, 'c, response, 'd) Pbrt_services.Server.rpc) ->
       (request, 'c, response, 'd) Pbrt_services.Server.rpc
       Pbrt_services.Server.t) =
  let service = server (fun t -> t) in
  let server = List.hd service.handlers in
  (module struct
    module Request = struct
      type t = request

      let encode t = encode client.encode_pb_req t
      let decode buffer = decode server.decode_pb_req buffer
    end

    module Response = struct
      type t = response

      let encode t = encode server.encode_pb_res t
      let decode buffer = decode client.decode_pb_res buffer
    end

    let package_name =
      match service.package with
      | [] -> None
      | _ :: _ as packages -> Some (String.concat "." packages)

    let service_name = service.service_name
    let method_name = server.name
  end : Grpc.Rpc.S
    with type Request.t = request
     and type Response.t = response)
