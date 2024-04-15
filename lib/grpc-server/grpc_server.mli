module Service : sig
  type 'handler t
  (** [t] represents a service. *)

  val v : unit -> 'handler t
  (** [v ()] creates a new service. *)

  val add_rpc : name:string -> rpc:'handler -> 'handler t -> 'handler t
  (** [add_rpc ~name ~rpc t] adds [rpc] to [t] and ensures that it is routable
      via [name]. *)
end

type error =
  [ `Not_found of
    [ `Service_not_found
    | `Rpc_not_found of string
    | `Invalid_url
    | `Bad_method ]
  | `Unsupported_media_type
  | `Not_acceptable
  | `Bad_request ]

type 'handler t
(** [t] represents a server and its associated services and routing information. *)

val v : unit -> 'handler t
(** [v ()] creates a new server. *)

val add_service :
  name:string -> service:'handler Service.t -> 'handler t -> 'handler t
(** [add_service ~name ~service t] adds [service] to [t] and ensures that it is
    routable via [name]. *)

val handle_request :
  'handler t ->
  is_post_request:bool ->
  get_header:(string -> string option) ->
  path:string ->
  ('handler, error) result
(** [handle_request t handler] handles a request using [handler] and the
    services registered in [t]. *)

(** Expert functionality. *)
module Expert : sig
  type 'handler rpc_handler = string -> ('handler, error) result

  val add_service : name:string -> service:'a rpc_handler -> 'a t -> 'a t
  (** [add_rpc ~name ~rpc t] adds [service] to [t] and ensures that it is
      routable via [name]. *)
end

type headers = { content_type : string; extra : (string * string) list }

val headers : ?extra:(string * string) list -> Grpc.Message.format -> headers
val headers_grpc_proto : headers

type trailers = {
  grpc_status : int;
  grpc_message : string option;
  extra : (string * string) list;
}

val make_trailers : ?extra:(string * string) list -> Grpc.Status.t -> trailers
val trailers_with_code : Grpc.Status.code -> trailers
