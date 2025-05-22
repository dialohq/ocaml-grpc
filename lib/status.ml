open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
[@@deriving show, yojson_of]

let int_of_code = function
  | OK -> 0
  | Cancelled -> 1
  | Unknown -> 2
  | Invalid_argument -> 3
  | Deadline_exceeded -> 4
  | Not_found -> 5
  | Already_exists -> 6
  | Permission_denied -> 7
  | Resource_exhausted -> 8
  | Failed_precondition -> 9
  | Aborted -> 10
  | Out_of_range -> 11
  | Unimplemented -> 12
  | Internal -> 13
  | Unavailable -> 14
  | Data_loss -> 15
  | Unauthenticated -> 16

let code_of_int = function
  | 0 -> OK
  | 1 -> Cancelled
  | 2 -> Unknown
  | 3 -> Invalid_argument
  | 4 -> Deadline_exceeded
  | 5 -> Not_found
  | 6 -> Already_exists
  | 7 -> Permission_denied
  | 8 -> Resource_exhausted
  | 9 -> Failed_precondition
  | 10 -> Aborted
  | 11 -> Out_of_range
  | 12 -> Unimplemented
  | 13 -> Internal
  | 14 -> Unavailable
  | 15 -> Data_loss
  | 16 -> Unauthenticated
  | _ -> Unknown

let yojson_of_exn exn = `String (Printexc.to_string exn)
let pp_exn = Eio.Exn.pp

type info = Message of string | Exn of exn [@@deriving show, yojson_of]
type t = { code : code; info : info option } [@@deriving show, yojson_of]
