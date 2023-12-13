val client_rpc :
  ('request, _, 'response, _) Pbrt_services.Client.rpc ->
  ('request, 'response) Grpc.Rpc.Client_rpc.t

val server_rpc :
  ('request, _, 'response, _) Pbrt_services.Server.rpc ->
  ('request, 'response, unit) Grpc.Rpc.Server_rpc.t
