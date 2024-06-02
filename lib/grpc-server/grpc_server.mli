type error =
  [ `Not_found of
    [ `Service_not_found
    | `Rpc_not_found of string
    | `Invalid_url
    | `Bad_method ]
  | `Unsupported_media_type
  | `Not_acceptable
  | `Bad_request ]

type parsed_request = { service : string; meth : string }

val parse_request :
  is_post_request:bool ->
  get_header:(string -> string option) ->
  path:string ->
  (parsed_request, error) result
(** [handle_request t handler] handles a request using [handler] and the
    services registered in [t]. *)

type headers = { content_type : string; extra : (string * string) list }

val headers : ?extra:(string * string) list -> Grpc.Message.format -> headers
val headers_grpc_proto : headers

type trailers = {
  grpc_status : int;
  grpc_message : string option;
  extra : (string * string) list;
}

val make_trailers : ?extra:(string * string) list -> Grpc.Status.t -> trailers
