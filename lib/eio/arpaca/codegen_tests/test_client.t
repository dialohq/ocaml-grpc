Simplest possible cram test
  $ arpaca-gen client route_guide.proto -o .
  $ cat RouteGuide.ml
  let get_feature ~sw ~io request =
    let response =
      Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
        ~method_name:"GetFeature"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (Route_guide.encode_pb_point request)
    in
    match response with
    | `Success ({ response = res; _ } as result) ->
        `Success
          {
            result with
            response =
              res.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_feature;
          }
    | ( `Premature_close _ | `Write_error _ | `Connection_error _
      | `Response_not_ok _ ) as rest ->
        rest
  
  let list_features ~sw ~io request handler =
    Client.Server_streaming.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"ListFeatures"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (Route_guide.encode_pb_rectangle request) (fun net_response ~read ->
        let responses =
          Seq.map
            (fun response ->
              response.Grpc_eio_core.Body_reader.consume
                Route_guide.decode_pb_feature)
            read
        in
        handler net_response responses)
  
  let record_route ~sw ~io handler =
    let response =
      Client.Client_streaming.call ~sw ~io ~service:"routeguide.RouteGuide"
        ~method_name:"RecordRoute"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (fun net_response ~writer ->
          let writer' req = writer (Route_guide.encode_pb_point request) in
          handler net_response ~writer:writer')
    in
    match response with
    | `Success ({ response = res; _ } as result) ->
        `Success
          {
            result with
            response =
              res.Grpc_eio_core.Body_reader.consume
                Route_guide.decode_pb_route_summary;
          }
    | ( `Premature_close _ | `Write_error _ | `Connection_error _
      | `Response_not_ok _ ) as rest ->
        rest
  
  let route_chat ~sw ~io handler =
    Client.Bidirectional_streaming.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"RouteChat"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun net_response ~writer ~read ->
        let writer' req = writer (Route_guide.encode_pb_route_note request) in
        let read' =
          Seq.map
            (fun response ->
              response.Grpc_eio_core.Body_reader.consume
                Route_guide.decode_pb_route_note)
            read
        in
        handler net_response ~writer:writer' ~read:read')

