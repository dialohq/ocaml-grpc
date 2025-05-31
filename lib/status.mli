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

val int_of_code : code -> int
val code_of_int : int -> code

type info = Message of string | Exn of (exn[@printer Eio.Exn.pp])
[@@deriving show]

type t = { code : code; info : info option } [@@deriving show]
