val rpc :
  client:('request, _, 'response, _) Pbrt_services.Client.rpc ->
  server:
    ((('request, 'c, 'response, 'd) Pbrt_services.Server.rpc ->
     ('request, 'c, 'response, 'd) Pbrt_services.Server.rpc) ->
    ('request, 'c, 'response, 'd) Pbrt_services.Server.rpc
    Pbrt_services.Server.t) ->
  ('request, 'response) Grpc.Rpc.t
