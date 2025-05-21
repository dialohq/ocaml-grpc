open Ocaml_protoc_compiler_lib
module Pt = Pb_parsing_parse_tree
module Tt = Pb_typing_type_tree

let find_imported_file include_dirs file_name =
  if Sys.file_exists file_name then file_name
  else
    let found_file =
      List.fold_left
        (fun found_file include_dir ->
          let try_file_name = Filename.concat include_dir file_name in
          match (found_file, Sys.file_exists try_file_name) with
          | None, true -> Some try_file_name
          | Some previous, true ->
              Printf.eprintf
                ("[Warning] Imported file %s found in 2 directories, "
               ^^ "picking: %s\n")
                file_name previous;
              found_file
          | _, false -> found_file)
        None include_dirs
    in

    match found_file with
    | None -> Pb_exception.import_file_not_found file_name
    | Some file_name -> file_name

let compile proto_file_name include_dirs unsigned_tag =
  (* parsing *)
  let protos =
    Pb_parsing.parse_file
      (fun file_name ->
        let file_name = find_imported_file include_dirs file_name in
        (file_name, Pb_util.read_file file_name))
      proto_file_name
  in

  (* file options can be overriden/added with command line arguments *)
  let protos =
    List.map
      (fun proto ->
        {
          proto with
          Pt.file_options = Pb_option.merge proto.Pt.file_options [];
        })
      protos
  in

  let proto_file_options =
    let main_proto = List.hd protos in
    main_proto.Pt.file_options
  in

  (* typing *)
  let typed_proto = Pb_typing.perform_typing protos in
  let all_typed_protos = List.flatten typed_proto.proto_types in

  (* Only get the types which are part of the given proto file
     (compilation unit) *)
  let typed_proto =
    {
      typed_proto with
      Tt.proto_types =
        List.filter
          (function
            | { Tt.file_name; _ } :: _ when file_name = proto_file_name -> true
            | _ -> false)
          typed_proto.proto_types;
    }
  in

  (* -- OCaml Backend -- *)
  let module BO = Pb_codegen_backend in
  let ocaml_proto =
    BO.compile ~unsigned_tag ~all_types:all_typed_protos typed_proto
  in
  (ocaml_proto, proto_file_options)

open Cmdliner

(* Validate the protobuf file *)
let validate_proto file =
  if Sys.file_exists (Sys.getcwd () ^ "/" ^ file) then Ok file
  else Error (`Msg (Printf.sprintf "The protobuf file %s does not exist." file))

(* Validate the output directory *)
let validate_output_dir dir =
  let open Sys in
  if file_exists dir && is_directory dir then Ok dir
  else
    Error
      (`Msg
         (Printf.sprintf
            "The output directory %s does not exist or is not a directory." dir))

let include_path =
  let doc = "Include path for protobuf file" in
  Arg.(value & opt_all string [] & info [ "I" ] ~docv:"DIR" ~doc)

let suffix =
  let doc = "Include path for protobuf file" in
  Arg.(value & opt string "" & info [ "s"; "suffix" ] ~docv:"SUFFIX" ~doc)

let output_path =
  let doc = "Output directory where the files will be written" in
  Arg.(
    required
    & opt (some (conv (validate_output_dir, Format.pp_print_string))) (Some ".")
    & info [ "o" ] ~docv:"DIR" ~doc)

(* Protobuf file argument *)
let proto_file =
  let doc = "Protobuf file to process" in
  Arg.(
    required
    & pos 0 (some (conv (validate_proto, Format.pp_print_string))) None
    & info [] ~docv:"PROTO" ~doc)

let prepare proto_file_name include_dirs =
  let { Pb_codegen_ocaml_type.proto_services; _ }, _ =
    compile proto_file_name include_dirs false
  in
  let proto_file_name =
    if not (String.contains proto_file_name '/') then proto_file_name
    else List.hd @@ List.rev @@ String.split_on_char '/' proto_file_name
  in
  let proto_gen_module =
    Pb_codegen_util.caml_file_name_of_proto_file_name ~proto_file_name
  in
  (proto_services, proto_gen_module)

(* Client command *)
let client_cmd =
  let doc = "Generate client-side stubs." in
  let info = Cmd.info "client" ~doc in
  let term =
    Term.(
      const (fun proto includes output suffix ->
          let proto_services, proto_gen_module = prepare proto includes in
          List.iter
            (fun svc ->
              let scope = Pb_codegen_formatting.empty_scope () in
              Codegen.gen_service_client_struct ~proto_gen_module svc scope;

              let out =
                Out_channel.open_text
                  (output ^ "/" ^ svc.service_name ^ suffix ^ ".ml")
              in

              Pb_codegen_all.F.output out scope)
            proto_services)
      $ proto_file $ include_path $ output_path $ suffix)
  in
  Cmd.v info term

(* Server command *)
let server_cmd =
  let doc = "Generate server side stubs" in
  let info = Cmd.info "server" ~doc in
  let term =
    Term.(
      const (fun proto includes output suffix ->
          let proto_services, proto_gen_module = prepare proto includes in
          List.iter
            (fun svc ->
              let scope = Pb_codegen_formatting.empty_scope () in
              Codegen.gen_service_server_struct ~proto_gen_module svc scope;

              let out =
                Out_channel.open_text
                  (output ^ "/" ^ svc.service_name ^ suffix ^ ".ml")
              in

              Pb_codegen_all.F.output out scope)
            proto_services)
      $ proto_file $ include_path $ output_path $ suffix)
  in
  Cmd.v info term

(* Main command *)
let cmds = [ client_cmd; server_cmd ]

let () =
  let doc = "A command-line tool with client and server modes." in
  let info = Cmd.info "command" ~doc in
  exit (Cmd.eval (Cmd.group info cmds))
