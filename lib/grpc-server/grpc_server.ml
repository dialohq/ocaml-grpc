module StringMap = Map.Make (String)

module Service = struct
  module RpcMap = StringMap

  type 'handler t = 'handler RpcMap.t

  let v () = RpcMap.empty
  let add_rpc ~name ~rpc t = RpcMap.add name rpc t
end

module ServiceMap = StringMap

type error =
  [ `Not_found of
    [ `Service_not_found
    | `Rpc_not_found of string
    | `Invalid_url
    | `Bad_method ]
  | `Unsupported_media_type
  | `Not_acceptable
  | `Bad_request ]

type 'handler rpc_handler = string -> ('handler, error) result
type 'handler t = 'handler rpc_handler ServiceMap.t

let v () = ServiceMap.empty

module Expert = struct
  type nonrec 'a rpc_handler = 'a rpc_handler

  let add_service ~name ~service (t : 'a t) = ServiceMap.add name service t
end

let add_service ~name ~service t =
  Expert.add_service ~name
    ~service:(fun rpc_name ->
      match StringMap.find_opt rpc_name service with
      | Some rpc -> Ok rpc
      | None -> Error (`Not_found (`Rpc_not_found rpc_name)))
    t

let rec service_name_and_method = function
  | [] -> None
  | [ _ ] -> None
  | [ service_name; method_name ] -> Some (service_name, method_name)
  | _ :: tl -> service_name_and_method tl

let handle_request (t : 'handler t) ~is_post_request ~get_header ~path :
    ('handler, error) result =
  let route () =
    let parts = String.split_on_char '/' path in
    match service_name_and_method parts with
    | Some (service, rpc) -> (
        match ServiceMap.find_opt service t with
        | Some service -> service rpc
        | None -> Error (`Not_found `Service_not_found))
    | None -> Error (`Not_found `Invalid_url)
  in
  match is_post_request with
  | true -> (
      match get_header "content-type" with
      | Some s ->
          if
            Stringext.chop_prefix s ~prefix:"application/grpc" |> Option.is_some
          then
            match get_header "grpc-encoding" with
            | None | Some "identity" -> (
                match get_header "grpc-accept-encoding" with
                | None -> route ()
                | Some encodings ->
                    let encodings = String.split_on_char ',' encodings in
                    if List.mem "identity" encodings then route ()
                    else
                      (* TODO: respond with unimplemented *)
                      Error `Not_acceptable)
            | Some _ ->
                (* TODO: not sure if there is a specific way to handle this in grpc *)
                Error `Bad_request
          else Error `Unsupported_media_type
      | None -> Error `Unsupported_media_type)
  | _ -> Error (`Not_found `Bad_method)

type headers = { content_type : string; extra : (string * string) list }
type format = [ `Json | `Proto | `Other of string ]

let headers ?(extra = []) (format : format) =
  {
    content_type =
      (match format with
      | `Json -> "application/grpc+json"
      | `Proto -> "application/grpc+proto"
      | `Other s -> Printf.sprintf "application/grpc+%s" s);
    extra;
  }

let headers_grpc_proto = headers `Proto

type trailers = {
  grpc_status : int;
  grpc_message : string option;
  extra : (string * string) list;
}

let make_trailers ?(extra = []) status =
  {
    grpc_status = Grpc.Status.int_of_code (Grpc.Status.code status);
    grpc_message = Grpc.Status.message status;
    extra;
  }

let trailers_with_code code =
  {
    grpc_status = Grpc.Status.int_of_code code;
    grpc_message = None;
    extra = [];
  }
