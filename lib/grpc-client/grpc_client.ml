type request_headers = { content_type : string; te : string }

let make_request_headers ?(te = []) format =
  {
    content_type = Grpc.Message.format_to_content_type format;
    te =
      (match te with
      | [] -> "trailers"
      | te -> Printf.sprintf "trailers; %s" (String.concat "; " te));
  }

let make_path ~service ~method_name =
  Printf.sprintf "/%s/%s" service method_name

let status_of_trailers ~get_header =
  match get_header "grpc-status" with
  | None ->
      Grpc.Status.make ~error_message:"Server did not return grpc-status"
        Grpc.Status.Unknown
  | Some s -> (
      match Option.bind (int_of_string_opt s) Grpc.Status.code_of_int with
      | None ->
          Grpc.Status.make
            ~error_message:
              (Printf.sprintf "Server returned an invalid grpc-status %s" s)
            Grpc.Status.Unknown
      | Some status ->
          Grpc.Status.make ?error_message:(get_header "grpc-message") status)

let trailers_missing_status =
  Grpc.Status.make ~error_message:"Trailers missing" Grpc.Status.Unknown
