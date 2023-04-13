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

val v : ?message:string -> code -> t
(** [v ~message code] creates a new status with the given [code] and [message]. *)

val code : t -> code
(** [code t] returns the code associated with [t]. *)

val message : t -> string option
(** [message t] returns the message associated with [t], if there is one. *)

val extract_status : H2.Headers.t -> t
(** [extract_status headers] returns the status embedded in the headers, or a default
    when the status is invalid or missing. *)
