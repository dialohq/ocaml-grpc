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
  | 0 -> Some OK
  | 1 -> Some Cancelled
  | 2 -> Some Unknown
  | 3 -> Some Invalid_argument
  | 4 -> Some Deadline_exceeded
  | 5 -> Some Not_found
  | 6 -> Some Already_exists
  | 7 -> Some Permission_denied
  | 8 -> Some Resource_exhausted
  | 9 -> Some Failed_precondition
  | 10 -> Some Aborted
  | 11 -> Some Out_of_range
  | 12 -> Some Unimplemented
  | 13 -> Some Internal
  | 14 -> Some Unavailable
  | 15 -> Some Data_loss
  | 16 -> Some Unauthenticated
  | _ -> None

type t = { code : code; message : string option } [@@deriving show]

let make ?error_message code = { code; message = error_message }
let code t = t.code

let error_message t =
  Option.map (fun message -> Uri.pct_encode message) t.message

let extract_status ~get_header =
  let code, message =
    match get_header "grpc-status" with
    | None -> (Unknown, Some "Expected gprc-status header, got nothing")
    | Some s -> (
        match int_of_string_opt s with
        | None ->
            let msg =
              Printf.sprintf "Expected valid gprc-status header, got %s" s
            in
            (Unknown, Some msg)
        | Some i -> (
            match code_of_int i with
            | None ->
                let msg =
                  Printf.sprintf "Expected valid gprc-status code, got %i" i
                in
                (Unknown, Some msg)
            | Some c -> (c, get_header "grpc-message")))
  in
  make ?error_message:message code
