type format = [ `Json | `Proto | `Other of string ]

let format_to_content_type = function
  | `Json -> "application/grpc+json"
  | `Proto -> "application/grpc+proto"
  | `Other s -> Printf.sprintf "application/grpc+%s" s

let fill_header_cs ~length (buffer : Cstruct.t) =
  Cstruct.set_char buffer 0 '\x00';
  Cstruct.BE.set_uint16 buffer 1 (length lsr 16);
  Cstruct.BE.set_uint16 buffer 3 (length land 0xFFFF)

type request_headers = { content_type : string; te : string }

let make_request_headers ?(te = []) format =
  {
    content_type = format_to_content_type format;
    te =
      (match te with
      | [] -> "trailers"
      | te -> Printf.sprintf "trailers; %s" (String.concat "; " te));
  }

let make_path ~service ~method_name =
  Printf.sprintf "/%s/%s" service method_name
