open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Etcd.Etcdserverpb

let persistent_connection = ref None

let create_connection ~host ~port =
  Lwt_unix.getaddrinfo host (string_of_int port) [] >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  match addresses with
  | { Unix.ai_addr; _ } :: _ ->
      Lwt_unix.connect socket ai_addr >>= fun () ->
      H2_lwt_unix.Client.create_connection ~error_handler:ignore socket
  | _ -> assert false

let connection ~host ~port =
  match !persistent_connection with
  | Some connection when H2_lwt_unix.Client.is_closed connection = false ->
      print_endline "Reusing existing connection.";
      Lwt.return connection
  | _ ->
      create_connection ~host ~port >>= fun connection ->
      print_endline "Connection established.";
      persistent_connection := Some connection;
      Lwt.return connection

let do_grpc ~host ~port ~service ~rpc ~request ~decode ~show =
  let f response =
    response >>= function
    | Some response ->
        Lwt.return
          (match decode response with
          | Ok value -> Ok (Some (show value))
          | Error _ -> Error "decoding error")
    | None -> Lwt.return (Ok None)
  in
  let handler = Grpc_lwt.Client.Rpc.unary ~f request in
  connection ~host ~port >>= fun connection ->
  Grpc_lwt.Client.call ~service ~rpc ~scheme:"http" ~handler
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ()

let post ~key ~value =
  let request =
    PutRequest.make ~key:(Bytes.of_string key) ~value:(Bytes.of_string value) ()
    |> PutRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents
  in
  let decode response =
    Ocaml_protoc_plugin.Reader.create response |> PutResponse.from_proto
  in
  do_grpc ~service:"etcdserverpb.KV" ~rpc:"Put" ~request ~decode
    ~show:PutResponse.show

let get ~key =
  let request =
    RangeRequest.make ~key:(Bytes.of_string key) ()
    |> RangeRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents
  in
  let decode response =
    Ocaml_protoc_plugin.Reader.create response |> RangeResponse.from_proto
  in
  do_grpc ~service:"etcdserverpb.KV" ~rpc:"Range" ~request ~decode
    ~show:RangeResponse.show

let server ~etcd_host ~etcd_port ~port =
  let callback _conn req body =
    let key = req |> Request.uri |> Uri.path in
    let meth = req |> Request.meth |> Code.string_of_method in
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    (match meth with
    | "GET" -> get ~host:etcd_host ~port:etcd_port ~key
    | "POST" -> post ~host:etcd_host ~port:etcd_port ~key ~value:body
    | _ -> Lwt.return (Error `Method_not_allowed))
    >>= function
    | Ok (Ok ret, status) ->
        Format.printf "Success status: %a@." Grpc.Status.pp status;
        let body =
          match ret with
          | Some ret -> Format.asprintf "%s\n%a\n" ret Grpc.Status.pp status
          | None -> Format.asprintf "%a\n" Grpc.Status.pp status
        in
        Server.respond_string ~status:`OK ~body ()
    | Ok (Error error, status) ->
        Format.printf "Status: %a@." Grpc.Status.pp status;
        let body = Format.asprintf "%s\n%a\n" error Grpc.Status.pp status in
        Server.respond_string ~status:`Internal_server_error ~body ()
    | Error status ->
        Format.printf "HTTP/2 error status: %a@." H2.Status.pp_hum status;
        let body = Format.asprintf "%a\n" H2.Status.pp_hum status in
        let status = H2.Status.to_code status |> Cohttp.Code.status_of_code in
        Server.respond_string ~status ~body ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () =
  let etcd_host, etcd_port, port =
    try (Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3))
    with Invalid_argument _ ->
      Printf.eprintf "Usage: %s ETCD_HOST ETCD_PORT PORT\n" Sys.argv.(0);
      exit 1
  in
  Lwt_main.run (server ~etcd_host ~etcd_port ~port)
