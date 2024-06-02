type code =
  | OK
  | Cancelled
  | Unknown
  | Invalid_argument
  | Deadline_exceeded
  | Not_found
  | Already_exists
  | Permission_denied
  | Resource_exhausted
  | Failed_precondition
  | Aborted
  | Out_of_range
  | Unimplemented
  | Internal
  | Unavailable
  | Data_loss
  | Unauthenticated
[@@deriving show]

(** [code] represents the valid gRPC status codes to respond with. *)

val int_of_code : code -> int
(** [int_of_code c] returns the corresponding integer status code for [c]. *)

val code_of_int : int -> code option
(** [code_of_int i] returns the corresponding code for [i] if it exists. *)

type t [@@deriving show]
(** [t] represents a full gRPC status, this includes code and optional message. *)

val make : ?error_message:string -> code -> t
(** [v ~message code] creates a new status with the given [code] and [message].
    It is an error to construct an OK status with non-empty error_message *)

val code : t -> code
(** [code t] returns the code associated with [t]. *)

val error_message : t -> string option
(** [message t] returns the message associated with [t], if there is one. *)

val extract_status : get_header:(string -> string option) -> t
(** [extract_status ~get_header] returns the status embedded in the headers, or
    a default when the status is invalid or missing. *)
