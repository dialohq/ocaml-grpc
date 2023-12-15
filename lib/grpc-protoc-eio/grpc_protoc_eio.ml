open Pbrt_services.Value_mode

module Call = struct
  let unary (type request response) ?scheme ?headers
      (rpc : (request, unary, response, unary) Pbrt_services.Client.rpc)
      ~do_request request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc.client_rpc rpc)
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.unary request ~f)
      ()

  let client_streaming (type request response) ?scheme ?headers
      (rpc : (request, stream, response, unary) Pbrt_services.Client.rpc)
      ~do_request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc.client_rpc rpc)
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.client_streaming ~f)
      ()

  let server_streaming (type request response) ?scheme ?headers
      (rpc : (request, unary, response, stream) Pbrt_services.Client.rpc)
      ~do_request request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc.client_rpc rpc)
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.server_streaming request ~f)
      ()

  let bidirectional_streaming (type request response) ?scheme ?headers
      (rpc : (request, stream, response, stream) Pbrt_services.Client.rpc)
      ~do_request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc.client_rpc rpc)
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.bidirectional_streaming ~f)
      ()
end

module Implement = struct
  type rpc = unit Grpc_eio.Server.Typed_rpc.t

  let unary (type request response)
      (rpc : (request, unary, response, unary) Pbrt_services.Server.rpc) ~f =
    Grpc_eio.Server.Typed_rpc.unary (Grpc_protoc.server_rpc rpc) ~f

  let client_streaming (type request response)
      (rpc : (request, stream, response, unary) Pbrt_services.Server.rpc) ~f =
    Grpc_eio.Server.Typed_rpc.client_streaming (Grpc_protoc.server_rpc rpc) ~f

  let server_streaming (type request response)
      (rpc : (request, unary, response, stream) Pbrt_services.Server.rpc) ~f =
    Grpc_eio.Server.Typed_rpc.server_streaming (Grpc_protoc.server_rpc rpc) ~f

  let bidirectional_streaming (type request response)
      (rpc : (request, stream, response, stream) Pbrt_services.Server.rpc) ~f =
    Grpc_eio.Server.Typed_rpc.bidirectional_streaming
      (Grpc_protoc.server_rpc rpc)
      ~f

  let server { Pbrt_services.Server.package; service_name; handlers } =
    Grpc_eio.Server.Typed_rpc.server
      (Handlers_and_service_spec
         { service_spec = { package; service_name }; handlers })
end
