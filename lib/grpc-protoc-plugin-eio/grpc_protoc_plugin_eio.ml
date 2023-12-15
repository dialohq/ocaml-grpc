module type S = Ocaml_protoc_plugin.Service.Rpc

module Call = struct
  let unary (type request response) ?scheme ?headers
      (module R : S with type Request.t = request and type Response.t = response)
      ~do_request request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc_plugin.client_rpc (module R))
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.unary request ~f)
      ()

  let client_streaming (type request response) ?scheme ?headers
      (module R : S with type Request.t = request and type Response.t = response)
      ~do_request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc_plugin.client_rpc (module R))
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.client_streaming ~f)
      ()

  let server_streaming (type request response) ?scheme ?headers
      (module R : S with type Request.t = request and type Response.t = response)
      ~do_request request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc_plugin.client_rpc (module R))
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.server_streaming request ~f)
      ()

  let bidirectional_streaming (type request response) ?scheme ?headers
      (module R : S with type Request.t = request and type Response.t = response)
      ~do_request ~f =
    Grpc_eio.Client.Typed_rpc.call
      (Grpc_protoc_plugin.client_rpc (module R))
      ?scheme ~do_request ?headers
      ~handler:(Grpc_eio.Client.Typed_rpc.bidirectional_streaming ~f)
      ()
end

module Implement = struct
  type rpc = Grpc.Rpc.Service_spec.t Grpc_eio.Server.Typed_rpc.t

  let unary (type request response)
      (module R : S with type Request.t = request and type Response.t = response)
      ~f =
    Grpc_eio.Server.Typed_rpc.unary
      (Grpc_protoc_plugin.server_rpc (module R))
      ~f

  let client_streaming (type request response)
      (module R : S with type Request.t = request and type Response.t = response)
      ~f =
    Grpc_eio.Server.Typed_rpc.client_streaming
      (Grpc_protoc_plugin.server_rpc (module R))
      ~f

  let server_streaming (type request response)
      (module R : S with type Request.t = request and type Response.t = response)
      ~f =
    Grpc_eio.Server.Typed_rpc.server_streaming
      (Grpc_protoc_plugin.server_rpc (module R))
      ~f

  let bidirectional_streaming (type request response)
      (module R : S with type Request.t = request and type Response.t = response)
      ~f =
    Grpc_eio.Server.Typed_rpc.bidirectional_streaming
      (Grpc_protoc_plugin.server_rpc (module R))
      ~f

  let server handlers = Grpc_eio.Server.Typed_rpc.server (Handlers { handlers })
end
