module StringMap = Map.Make (String)

type error =
  [ `Not_found of [ `Service_not_found | `Invalid_url | `Bad_method ]
  | `Unsupported_media_type
  | `Bad_request
  | `Grpc of Grpc.Status.t ]

let error_to_code_and_headers error =
  match error with
  | `Not_found _ -> (404, [])
  | `Unsupported_media_type -> (415, [])
  | `Bad_request -> (400, [])
  | `Grpc status -> Grpc.Status.to_net_resp status

let rec service_name_and_method = function
  | [] -> None
  | [ _ ] -> None
  | [ service_name; method_name ] -> Some (service_name, method_name)
  | _ :: tl -> service_name_and_method tl

type parsed_request = { service : string; meth : string }

let parse_request ~is_post_request ~get_header ~path :
    (parsed_request, error) result =
  let route () =
    let parts = String.split_on_char '/' path in
    match service_name_and_method parts with
    | Some (service, meth) -> Ok { service; meth }
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
                    else Error (`Grpc (Grpc.Status.make Unimplemented)))
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
    grpc_message = Grpc.Status.error_message status;
    extra;
  }
