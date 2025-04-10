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
  let gen_exn_rpc sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let rpc_name = rpc.rpc_name in
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        F.linep sc
          {|let %s (type headers net_response stream_error connection_error) ~sw ~(io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) request =
  let response =
    Grpc_client_eio.Client.Unary.call ~sw ~io ~service:"%s"
      ~method_name:%S
      ~headers:(Grpc_client.make_request_headers `Proto)
      (%s.%s request)
  in
  let (module Io') = io in
  match response with
  | `Success ({ response = res; _ } as result) ->
      {
        result with
        response =
          res.Grpc_eio_core.Body_reader.consume %s.%s;
      }
  | #Grpc_client_eio.Rpc_error.Unary.error' as rest -> Io'.raise_client_error (Unary rest)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Server_streaming ->
        F.linep sc
          {|let %s (type headers net_response stream_error connection_error) ~sw ~(io :
      ( headers,
        net_response,
        Pbrt.Encoder.t ->unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) request handler =
    let stream =
      Grpc_client_eio.Client.Server_streaming.call ~sw ~io ~service:"%s"
        ~method_name:"%s"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (%s.%s request) (fun net_response ~read ->
          let responses =
            Seq.map
              (fun response ->
                response.Grpc_eio_core.Body_reader.consume
                  %s.%s)
              read
          in
          let (module Io') = io in
          handler net_response responses)
    in
    let (module Io') = io in
    match stream with
    | `Stream_result_success result -> result
    | #Grpc_client_eio.Rpc_error.Server_streaming.error' as rest -> Io'.raise_client_error (Server_streaming rest)
|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Client_streaming ->
        F.linep sc
          {|let %s (type headers net_response stream_error connection_error) ~sw ~(io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) handler =
  let response =
    Grpc_client_eio.Client.Client_streaming.call ~sw ~io ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun net_response ~writer ->
        let writer' req = writer.write (%s.%s req) in
        handler net_response ~writer:writer')
  in
  let (module Io') = io in
  match response with
  | `Success ({ response = res; _ } as result) ->
      {
        result with
        response =
          res.Grpc_eio_core.Body_reader.consume
            %s.%s;
      }
  | #Grpc_client_eio.Rpc_error.Client_streaming.error' as rest -> Io'.raise_client_error (Client_streaming rest)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Bidirectional_streaming ->
        F.linep sc
          {|let %s (type headers net_response stream_error connection_error) ~sw ~(io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) handler =
    let stream =
      Grpc_client_eio.Client.Bidirectional_streaming.call ~sw ~io ~service:"%s"
        ~method_name:"%s"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (fun net_response ~writer ~read ->
          let writer' req = writer.write (%s.%s req) in
          let read' =
            Seq.map
              (fun response ->
                response.Grpc_eio_core.Body_reader.consume
                  %s.%s)
              read
          in
          handler net_response ~writer:writer' ~read:read')
    in 
    let (module Io') = io in
    match stream with
    | `Stream_result_success result -> result
    | #Grpc_client_eio.Rpc_error.Bidirectional_streaming.error' as rest -> Io'.raise_client_error (Bidirectional_streaming rest)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
  in
  let gen_result_rpc sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let rpc_name = rpc.rpc_name in
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        F.linep sc
          {|let %s ~sw ~io request =
  let response =
    Grpc_client_eio.Client.Unary.call ~sw ~io ~service:"%s"
      ~method_name:%S
      ~headers:(Grpc_client.make_request_headers `Proto)
      (%s.%s request)
  in
  match response with
  | `Success ({ response = res; _ } as result) ->
      Ok
        {
          result with
          response =
            res.Grpc_eio_core.Body_reader.consume %s.%s;
        }
  | #Grpc_client_eio.Rpc_error.Unary.error' as rest -> Error rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Server_streaming ->
        F.linep sc
          {|let %s ~sw ~io request handler =
    let stream =
      Grpc_client_eio.Client.Server_streaming.call ~sw ~io ~service:"%s"
        ~method_name:"%s"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (%s.%s request) (fun net_response ~read ->
          let responses =
            Seq.map
              (fun response ->
                response.Grpc_eio_core.Body_reader.consume
                  %s.%s)
              read
          in
          handler net_response responses)
    in 
    match stream with
    | `Stream_result_success result -> Ok result
    | #Grpc_client_eio.Rpc_error.Server_streaming.error' as rest -> Error rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Client_streaming ->
        F.linep sc
          {|let %s ~sw ~io handler =
  let response =
    Grpc_client_eio.Client.Client_streaming.call ~sw ~io ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun net_response ~writer ->
        let writer' req = writer.write (%s.%s req) in
        handler net_response ~writer:writer')
  in
  match response with
  | `Success ({ response = res; _ } as result) ->
      Ok
        {
          result with
          response =
            res.Grpc_eio_core.Body_reader.consume
              %s.%s;
        }
  | #Grpc_client_eio.Rpc_error.Client_streaming.error' as rest -> Error rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Bidirectional_streaming ->
        F.linep sc
          {|let %s ~sw ~io handler =
    let stream =
      Grpc_client_eio.Client.Bidirectional_streaming.call ~sw ~io ~service:"%s"
        ~method_name:"%s"
        ~headers:(Grpc_client.make_request_headers `Proto)
        (fun net_response ~writer ~read ->
          let writer' req = writer.write (%s.%s req) in
          let read' =
            Seq.map
              (fun response ->
                response.Grpc_eio_core.Body_reader.consume
                  %s.%s)
              read
          in
          handler net_response ~writer:writer' ~read:read')
    in
    match stream with
    | `Stream_result_success result -> Ok result
    | #Grpc_client_eio.Rpc_error.Bidirectional_streaming.error' as rest -> Error rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
  in
  let gen_expert_rpc sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let rpc_name = rpc.rpc_name in
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        F.linep sc
          {|let %s ~sw ~io request =
  let response =
    Grpc_client_eio.Client.Unary.call ~sw ~io ~service:"%s.%s"
      ~method_name:%S
      ~headers:(Grpc_client.make_request_headers `Proto)
      (%s.%s request)
  in
  match response with
  | `Success ({ response = res; _ } as result) ->
      `Success
        {
          result with
          response =
            res.Grpc_eio_core.Body_reader.consume %s.%s;
        }
  | #Grpc_client_eio.Rpc_error.Unary.error' as rest ->
      rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          service.service_name rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Server_streaming ->
        F.linep sc
          {|let %s ~sw ~io request handler =
  Grpc_client_eio.Client.Server_streaming.call ~sw ~io ~service:"%s"
    ~method_name:"%s"
    ~headers:(Grpc_client.make_request_headers `Proto)
    (%s.%s request) (fun net_response ~read ->
      let responses =
        Seq.map
          (fun response ->
            response.Grpc_eio_core.Body_reader.consume
              %s.%s)
          read
      in
      handler net_response responses)|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Client_streaming ->
        F.linep sc
          {|let %s ~sw ~io handler =
  let response =
    Grpc_client_eio.Client.Client_streaming.call ~sw ~io ~service:"%s"
      ~method_name:"%s"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun net_response ~writer ->
        let writer' req = writer.write (%s.%s req) in
        handler net_response ~writer:writer')
  in
  match response with
  | `Success ({ response = res; _ } as result) ->
      `Success
        {
          result with
          response =
            res.Grpc_eio_core.Body_reader.consume
              %s.%s;
        }
  | #Grpc_client_eio.Rpc_error.Client_streaming.error' as rest ->
      rest|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
    | `Bidirectional_streaming ->
        F.linep sc
          {|let %s ~sw ~io handler =
  Grpc_client_eio.Client.Bidirectional_streaming.call ~sw ~io ~service:"%s"
    ~method_name:"%s"
    ~headers:(Grpc_client.make_request_headers `Proto)
    (fun net_response ~writer ~read ->
      let writer' req = writer.write (%s.%s req) in
      let read' =
        Seq.map
          (fun response ->
            response.Grpc_eio_core.Body_reader.consume
              %s.%s)
          read
      in
      handler net_response ~writer:writer' ~read:read')|}
          (Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case)
          (service_name_of_package service.service_packages service.service_name)
          rpc.rpc_name typ_mod_name
          (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req)
          typ_mod_name
          (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res)
  in
  List.iteri (gen_exn_rpc sc) service.service_body;
  F.empty_line sc;
  F.linep sc "module Result = struct";
  F.empty_line sc;
  List.iteri (gen_result_rpc sc) service.service_body;
  F.linep sc "end";
  F.empty_line sc;
  F.linep sc "module Expert = struct";
  F.empty_line sc;
  List.iteri (gen_expert_rpc sc) service.service_body;
  F.linep sc "end"

let gen_service_server_struct ~proto_gen_module (service : Ot.service) top_scope
    : unit =
  let typ_mod_name = String.capitalize_ascii proto_gen_module in
  let gen_rpc_sig sc i (rpc : Ot.rpc) =
    if i > 0 then F.empty_line sc;
    let name = Pb_codegen_util.function_name_of_rpc rpc in

    F.linep sc "val %s :" (to_snake_case name);
    (* F.linep sc *)
    (*   "  Eio.Net.Sockaddr.stream * H2.Reqd.t * H2.Request.t * H2.Reqd.error \ *)
    (*    Eio.Promise.t ->"; *)
    F.linep sc "  net_request ->";
    let req_type =
      Printf.sprintf "%s.%s" typ_mod_name (ocaml_type_of_rpc_type rpc.rpc_req)
    in
    let res_type =
      Printf.sprintf "%s.%s" typ_mod_name (ocaml_type_of_rpc_type rpc.rpc_res)
    in
    match rpc_kind rpc.rpc_req rpc.rpc_res with
    | `Unary ->
        F.linep sc {|  %s ->
    %s * (string * string) list|} req_type res_type
    | `Client_streaming ->
        F.linep sc
          {|  %s Seq.t ->
    %s * (string * string) list|}
          req_type res_type
    | `Server_streaming ->
        F.linep sc
          {|  %s ->
    (%s -> unit) ->
    (string * string) list|}
          req_type res_type
    | `Bidirectional_streaming ->
        F.linep sc
          {|  %s Seq.t ->
    (%s -> unit) ->
    (string * string) list|}
          req_type res_type
  in

  let gen_impl_sig sc =
    List.iteri (gen_rpc_sig sc) service.service_body
    (* now generate a function from the module type to a [Service_server.t] *)
  in

  let gen_rpc_handler sc (rpc : Ot.rpc) =
    let rpc_name = rpc.rpc_name in
    let service_name = service.service_name in

    F.linep sc {|| "%s", %S ->|}
      (String.concat "." (service.service_packages @ [ service.service_name ]))
      rpc.rpc_name;
    let impl = Pb_codegen_util.function_name_of_rpc rpc |> to_snake_case in

    let decoder_func =
      Printf.sprintf "%s.%s" typ_mod_name
        (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_req)
    in
    let encoder_func =
      Printf.sprintf "%s.%s" typ_mod_name
        (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_res)
    in

    let p = F.linep in
    let sub = F.sub_scope in

    sub sc (fun sc ->
        p sc {|fun req { Grpc_server_eio.Rpc.accept } ->|};
        sub sc (fun sc ->
            p sc {|accept Grpc_server.headers_grpc_proto|};
            sub sc (fun sc ->
                match rpc_kind rpc.rpc_req rpc.rpc_res with
                | `Unary ->
                    p sc {|(Grpc_server_eio.Rpc.unary (fun grpc_req ->|};
                    F.line sc {|let response, trailers =|};
                    sub sc (fun sc ->
                        p sc
                          {|Impl.%s req (grpc_req.Grpc_eio_core.Body_reader.consume %s)|}
                          impl decoder_func);
                    F.line sc "in";
                    p sc {|((%s response), trailers )))|} encoder_func
                | `Client_streaming ->
                    p sc
                      {|(Grpc_server_eio.Rpc.client_streaming (fun grpc_req_seq ->|};
                    p sc {|let response, trailers =|};
                    sub sc (fun sc ->
                        p sc {|Impl.%s req|} impl;
                        sub sc (fun sc ->
                            p sc {|(Seq.map (fun grpc_req ->|};
                            sub sc (fun sc ->
                                p sc
                                  {|grpc_req.Grpc_eio_core.Body_reader.consume %s|}
                                  decoder_func);
                            p sc {|) grpc_req_seq)|}));
                    p sc "in";
                    p sc {|((%s response), trailers)))|} encoder_func
                | `Server_streaming ->
                    p sc
                      {|(Grpc_server_eio.Rpc.server_streaming (fun grpc_req write ->|};
                    p sc {|let trailers =|};
                    sub sc (fun sc ->
                        p sc {|Impl.%s req|} impl;
                        sub sc (fun sc ->
                            p sc
                              {|(grpc_req.Grpc_eio_core.Body_reader.consume %s)|}
                              decoder_func;
                            p sc {|(fun resp -> write (%s resp))|} encoder_func));
                    p sc "in";
                    p sc {|trailers))|}
                | `Bidirectional_streaming ->
                    p sc {|(fun grpc_req_seq write ->|};
                    p sc {|let trailers =|};
                    sub sc (fun sc ->
                        p sc {|Impl.%s req|} impl;
                        sub sc (fun sc ->
                            p sc
                              {|(Seq.map (fun grpc_req -> grpc_req.Grpc_eio_core.Body_reader.consume %s) grpc_req_seq)|}
                              decoder_func;
                            p sc {|(fun resp -> write (%s resp))|} encoder_func));
                    p sc "in";
                    p sc {|trailers)|})))
  in

  let sc = top_scope in

  F.line sc "module type Implementation = sig";
  F.line sc "  type net_request";
  F.empty_line sc;
  F.sub_scope sc gen_impl_sig;
  F.line sc "end";
  F.empty_line sc;
  F.linep sc
    "let create_server (type net_request) (module Impl : Implementation with \
     type net_request = net_request) ~service ~meth =";
  F.sub_scope sc (fun sc ->
      F.linep sc "match (service, meth) with";
      List.iter (gen_rpc_handler sc) service.service_body;
      F.linep sc
        {|| _ ->
    raise (Grpc_server_eio.Server_error (Grpc.Status.make Unimplemented, []))|})
