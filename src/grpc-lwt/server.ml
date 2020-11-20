module Server : Grpc.Server.S = struct
  module ServiceMap = Map.Make (String)

  type t = { mutable services : (module Grpc.Service.S) ServiceMap.t }

  let v () = { services = ServiceMap.empty }

  let add_service (module Service : Grpc.Service.S) t =
    t.services <-
      ServiceMap.add Service.name (module Service : Grpc.Service.S) t.services

  let add_services services t =
    List.iter (fun service -> add_service service t) services

  let handle_request t reqd =
    let request = H2.Reqd.request reqd in
    let respond_with code =
      H2.Reqd.respond_with_string reqd (H2.Response.create code) ""
    in
    let route () =
      let parts = String.split_on_char '/' request.target in
      if List.length parts > 1 then
        (* allow for arbitrary prefixes *)
        let service_name = List.nth parts (List.length parts - 2) in
        let service = ServiceMap.find_opt service_name t.services in
        match service with
        | Some (module Service) -> Service.handle_rpc reqd
        | None -> respond_with `Not_found
      else respond_with `Not_found
    in
    match request.meth with
    | `POST -> (
        match H2.Headers.get request.headers "content-type" with
        | Some s ->
            if
              Stringext.chop_prefix s ~prefix:"application/grpc"
              |> Option.is_some
            then
              match H2.Headers.get request.headers "grpc-encoding" with
              | None | Some "identity" -> (
                  match
                    H2.Headers.get request.headers "grpc-accept-encoding"
                  with
                  | None -> route ()
                  | Some encodings ->
                      let encodings = String.split_on_char ',' encodings in
                      if List.mem "identity" encodings then route ()
                      else respond_with `Not_acceptable )
              | Some _ ->
                  (* TODO: not sure if there is a specific way to handle this in grpc *)
                  respond_with `Bad_request
            else respond_with `Unsupported_media_type
        | None -> respond_with `Unsupported_media_type )
    | _ -> respond_with `Not_found
end
