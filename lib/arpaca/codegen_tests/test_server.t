Simplest possible cram test
  $ arpaca-gen server -o . --suffix _server  route_guide.proto 
  $ cat RouteGuide_server.ml
  module type Implementation = sig
    val get_feature :
      H2.Request.t ->
      Route_guide.point ->
      Route_guide.feature * (string * string) list
    
    val list_features :
      H2.Request.t ->
      Route_guide.rectangle ->
      (Route_guide.feature -> unit) ->
      (string * string) list
    
    val record_route :
      H2.Request.t ->
      Route_guide.point Seq.t ->
      Route_guide.route_summary * (string * string) list
    
    val route_chat :
      H2.Request.t ->
      Route_guide.route_note Seq.t ->
      (Route_guide.route_note -> unit) ->
      (string * string) list
  end
  
  let create_server (module Impl : Implementation) ~service ~meth =
    match (service, meth) with
    | "routeguide.RouteGuide", "GetFeature" ->
      fun req { Grpc_server_eio.Rpc.accept } ->
        accept Grpc_server.headers_grpc_proto
          (Grpc_server_eio.Rpc.unary (fun grpc_req ->
          let response, trailers =
            Impl.get_feature req (grpc_req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_point)
          in
          ((Route_guide.encode_pb_feature response), trailers )))
    | "routeguide.RouteGuide", "ListFeatures" ->
      fun req { Grpc_server_eio.Rpc.accept } ->
        accept Grpc_server.headers_grpc_proto
          (Grpc_server_eio.Rpc.server_streaming (fun grpc_req write ->
          let trailers =
            Impl.list_features req
              (grpc_req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_rectangle)
              (fun resp -> write (Route_guide.encode_pb_feature resp))
          in
          trailers))
    | "routeguide.RouteGuide", "RecordRoute" ->
      fun req { Grpc_server_eio.Rpc.accept } ->
        accept Grpc_server.headers_grpc_proto
          (Grpc_server_eio.Rpc.client_streaming (fun grpc_req_seq ->
          let response, trailers =
            Impl.record_route req
              (Seq.map (fun grpc_req ->
                grpc_req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_point
              ) grpc_req_seq)
          in
          ((Route_guide.encode_pb_route_summary response), trailers)))
    | "routeguide.RouteGuide", "RouteChat" ->
      fun req { Grpc_server_eio.Rpc.accept } ->
        accept Grpc_server.headers_grpc_proto
          (fun grpc_req_seq write ->
          let trailers =
            Impl.route_chat req
              (Seq.map (fun grpc_req -> grpc_req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_route_note) grpc_req_seq)
              (fun resp -> write (Route_guide.encode_pb_route_note resp))
          in
          trailers)
    | _ ->
      raise (Grpc_server_eio.Server_error (Grpc.Status.make Unimplemented, []))
