type request_headers = { content_type : string; te : string }

val make_request_headers :
  ?te:string list -> Grpc.Message.format -> request_headers

val make_path : service:string -> method_name:string -> string
val status_of_trailers : get_header:(string -> string option) -> Grpc.Status.t
val trailers_missing_status : Grpc.Status.t
