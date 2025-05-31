open Ocaml_protoc_compiler_lib
module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let ocaml_type_of_rpc_type (rpc : Ot.rpc_type) : string =
  match rpc with
  | Rpc_scalar ty -> Pb_codegen_util.string_of_field_type ty
  | Rpc_stream ty -> Pb_codegen_util.string_of_field_type ty

let rpc_kind (req : Ot.rpc_type) (res : Ot.rpc_type) =
  match (req, res) with
  | Rpc_scalar _, Rpc_scalar _ -> `Unary
  | Rpc_scalar _, Rpc_stream _ -> `Server_streaming
  | Rpc_stream _, Rpc_scalar _ -> `Client_streaming
  | Rpc_stream _, Rpc_stream _ -> `Bidirectional_streaming

let function_name_encode_pb ~service_name ~rpc_name (ty : Ot.rpc_type) : string
    =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun () enc -> Pbrt.Encoder.empty_nested enc)"
    | Ot.Ft_user_defined_type udt ->
        let function_prefix = "encode_pb" in
        Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
        Printf.eprintf "cannot binary-encode request for %s in service %s\n%!"
          rpc_name service_name;
        exit 1
  in
  match ty with Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let function_name_decode_pb ~service_name ~rpc_name (ty : Ot.rpc_type) : string
    =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun d -> Pbrt.Decoder.empty_nested d)"
    | Ot.Ft_user_defined_type udt ->
        let function_prefix = "decode_pb" in
        Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
        Printf.eprintf "cannot decode binary request for %s in service %s\n%!"
          rpc_name service_name;
        exit 1
  in
  match ty with Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let to_snake_case =
  let regex =
    Re.replace (Re.compile Re.upper) ~f:(fun g ->
        if Re.Group.start g 0 > 0 then
          "_" ^ String.lowercase_ascii (Re.Group.get g 0)
        else Re.Group.get g 0)
  in
  fun str -> regex str

let service_name_of_package service_packages service =
  String.concat "." (service_packages @ [ service ])

let gen_service_client_struct ~proto_gen_module (service : Ot.service) sc : unit
    =
  let typ_mod_name = String.capitalize_ascii proto_gen_module in
  let service_name = service.service_name in
  let gen_result_rpc sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let rpc_name = rpc.rpc_name in
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        F.linep sc
          {|let %s ~channel request =
  Grpc.Client.Unary.call 
    ~channel 
    ~service:"%s"
    ~method_name:%S
    ~headers:(Grpc.Utils.make_request_headers `Proto)
    (%s.%s request)
  |> Result.map %s.%s|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Server_streaming ->
        F.linep sc
          {|let %s ~channel request handler =
  Grpc.Client.Server_streaming.call 
    ~channel 
    ~service:"%s"
    ~method_name:"%s"
    ~headers:(Grpc.Utils.make_request_headers `Proto)
    (%s.%s request) 
    (fun ~reader -> 
        handler 
        ~reader:(Seq.map %s.%s reader))|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Client_streaming ->
        F.linep sc
          {|let %s ~channel handler =
  Grpc.Client.Client_streaming.call 
    ~channel 
    ~service:"%s"
    ~method_name:"%s"
    ~headers:(Grpc.Utils.make_request_headers `Proto)
    (fun ~writer -> 
        handler 
        ~writer:(function 
          | Some msg -> writer (Some (%s.%s msg))
          | None -> writer None))
  |> Result.map (fun (decoder, r) -> (%s.%s decoder, r))|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Bidirectional_streaming ->
        F.linep sc
          {|let %s ~channel handler =
  Grpc.Client.Bidirectional_streaming.call 
    ~channel 
    ~service:"%s"
    ~method_name:"%s"
    ~headers:(Grpc.Utils.make_request_headers `Proto)
    (fun ~writer ~reader -> 
        handler 
        ~writer:(function 
          | Some msg -> writer (Some (%s.%s msg))
          | None -> writer None)
        ~reader:(Seq.map %s.%s reader))|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
  in

  let gen_expert_rpc sc i (rpc : Ot.rpc) =
    let rpc_name = rpc.rpc_name in
    if i > 0 then F.empty_line sc;
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Server_streaming ->
        F.linep sc
          {|  let %s ~channel ~initial_context request reader =
    Grpc.Client.Server_streaming.Expert.call 
      ~channel
      ~initial_context 
      ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc.Utils.make_request_headers `Proto)
      (%s.%s request) 
      (fun c -> function
        | Some decoder -> reader c (%s.%s decoder)
        | None -> c)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Client_streaming ->
        F.linep sc
          {|  let %s ~channel ~initial_context writer =
    Grpc.Client.Client_streaming.Expert.call
      ~channel 
      ~initial_context 
      ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc.Utils.make_request_headers `Proto)
      (fun c -> 
           map_fst (%s.%s |> Option.map) (writer c))
    |> Result.map %s.%s|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Bidirectional_streaming ->
        F.linep sc
          {|  let %s ~channel ~initial_context writer reader =
    Grpc.Client.Bidirectional_streaming.Expert.call 
      ~channel 
      ~initial_context 
      ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc.Utils.make_request_headers `Proto)
      (fun c -> 
          map_fst (%s.%s |> Option.map) (writer c))
      (fun c -> function
        | Some decoder -> reader c (%s.%s decoder)
        | None -> c)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | _ -> ()
  in

  F.linep sc "open Grpc.Utils";
  F.empty_line sc;
  List.iteri (gen_result_rpc sc) service.service_body;
  F.empty_line sc;
  F.line sc "module Expert = struct";
  List.filter
    (fun rpc -> rpc_kind rpc.Ot.rpc_req rpc.rpc_res <> `Unary)
    service.service_body
  |> List.iteri (gen_expert_rpc sc);
  F.empty_line sc;
  F.line sc "end"

let gen_service_server_struct ~proto_gen_module (service : Ot.service) top_scope
    : unit =
  let sub = F.sub_scope in
  let p = F.linep in
  let el = F.empty_line in

  let typ_mod_name = String.capitalize_ascii proto_gen_module in
  let gen_rpc_sig sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let name = Pb_codegen_util.function_name_of_rpc rpc in

    p sc "module %s : sig" (String.capitalize_ascii name);
    let req_type =
      Printf.sprintf "%s.%s" typ_mod_name (ocaml_type_of_rpc_type rpc.rpc_req)
    in
    let res_type =
      Printf.sprintf "%s.%s" typ_mod_name (ocaml_type_of_rpc_type rpc.rpc_res)
    in
    sub sc (fun sc ->
        match rpc_kind rpc.rpc_req rpc.rpc_res with
        | `Unary -> p sc "val handler : %s -> %s" req_type res_type
        | `Client_streaming ->
            p sc "type context";
            el sc;
            p sc "val initial_context : context";
            p sc "val reader : (context -> %s -> context)" req_type;
            p sc "val respond : (context -> %s)" res_type
        | `Server_streaming ->
            p sc "type context";
            el sc;
            p sc "val initial_context : context";
            p sc "val handler : %s -> (context -> %s option * context)" req_type
              res_type
        | `Bidirectional_streaming ->
            p sc "type context";
            el sc;
            p sc "val initial_context : context";
            p sc "val reader : (context -> %s option -> context)" req_type;
            p sc "val writer : (context -> %s option * context)" res_type);
    p sc "end"
  in

  let gen_imperative_impl_sig sc =
    p sc "module type Implementation = sig";
    sub sc (fun sc -> List.iteri (gen_rpc_sig sc) service.service_body);
    p sc "end"
  in

  let gen_rpc_match sc (rpc : Ot.rpc) =
    let rpc_name = rpc.rpc_name in
    let service_name = service.service_name in
    let decoder_func =
      Printf.sprintf "%s.%s" typ_mod_name
        (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_req)
    in
    let impl =
      Printf.sprintf "Impl.%s"
      @@ (Pb_codegen_util.function_name_of_rpc rpc |> String.capitalize_ascii)
    in
    let encoder_func =
      Printf.sprintf "%s.%s" typ_mod_name
        (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_res)
    in

    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        p sc {|| "%s", %S ->|}
          (String.concat "." (service.service_packages @ [ service_name ]))
          rpc_name;
        sub sc (fun sc ->
            p sc "let handler decoder =";
            sub sc (fun sc ->
                p sc "%s decoder" decoder_func;
                p sc "|> %s.handler" impl;
                p sc "|> %s" encoder_func);
            p sc "in";
            p sc "Some (Unary.respond handler)")
    | `Server_streaming ->
        p sc {|| "%s", %S ->|}
          (String.concat "." (service.service_packages @ [ service_name ]))
          rpc_name;
        sub sc (fun sc ->
            p sc "let handler decoder =";
            sub sc (fun sc ->
                p sc "let f = %s decoder |> %s.handler in" decoder_func impl;
                p sc "fun c ->";
                sub sc (fun sc ->
                    p sc "map_fst (%s |> Option.map) (f c)" encoder_func));
            p sc "in";
            p sc "Some (ServerStreaming.respond %s.initial_context handler)"
              impl)
    | `Client_streaming ->
        p sc {|| "%s", %S ->|}
          (String.concat "." (service.service_packages @ [ service_name ]))
          rpc_name;
        sub sc (fun sc ->
            p sc "let reader, respond =";
            sub sc (fun sc ->
                p sc "let single_write = (fun c -> %s.respond c |> %s) in" impl
                  encoder_func;
                p sc
                  "let reader = (fun c -> function None -> c | Some d -> \
                   %s.reader c (%s d)) in"
                  impl decoder_func;
                p sc "(reader, single_write)");
            p sc "in";
            p sc
              "Some (ClientStreaming.respond %s.initial_context reader respond)"
              impl)
    | `Bidirectional_streaming ->
        p sc {|| "%s", %S ->|}
          (String.concat "." (service.service_packages @ [ service_name ]))
          rpc_name;
        sub sc (fun sc ->
            p sc "let reader, writer =";
            sub sc (fun sc ->
                p sc
                  "let writer = (fun c -> map_fst (%s |> Option.map) \
                   (%s.writer c)) in"
                  encoder_func impl;
                p sc
                  "let reader = (fun c d ->  %s.reader c (Option.map (%s) d)) \
                   in"
                  impl decoder_func;
                p sc "(reader, writer)");
            p sc "in";
            p sc
              "Some (BidirectionalStreaming.respond %s.initial_context reader \
               writer)"
              impl)
  in

  let gen_connection_handler (sc : F.scope) =
    p sc "let connection_handler (module Impl : Implementation) =";
    sub sc (fun sc ->
        p sc "let open Grpc.Server in";
        p sc {|let get_route : Grpc.Server.route_getter =|};
        sub sc (fun sc ->
            p sc {|fun ~service ~meth ->|};
            sub sc (fun sc ->
                p sc "match (service, meth) with";
                List.iter (gen_rpc_match sc) service.service_body;
                p sc "| _ -> None"));

        p sc "in";
        el sc;
        p sc "connection_handler get_route")
  in

  let sc = top_scope in

  F.linep sc "open Grpc.Utils";
  F.empty_line sc;
  gen_imperative_impl_sig sc;
  F.empty_line sc;
  gen_connection_handler sc
(* F.linep sc *)
(*   "let create_server (type net_request) (module Impl : Implementation with \ *)
  (*    type net_request = net_request) ~service ~meth =" *)
(* F.sub_scope sc (fun sc -> *)
(*     F.linep sc "match (service, meth) with"; *)
(*     List.iter (gen_rpc_handler sc) service.service_body; *)
(*     F.linep sc *)
(*       {|| _ -> *)
  (*   raise (Grpc_server_eio.Server_error (Grpc_utils.Status.make Unimplemented, []))|}) *)
