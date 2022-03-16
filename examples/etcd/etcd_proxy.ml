open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Etcd.Etcdserverpb

let etcd_host = "127.0.0.1"
let etcd_port = 2379
let cohttp_port = 8080
let persistent_connection = ref None

let create_connection () =
  Lwt_unix.getaddrinfo etcd_host (string_of_int etcd_port) []
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  match addresses with
  | { Unix.ai_addr; _ } :: _ ->
      Lwt_unix.connect socket ai_addr >>= fun () ->
      H2_lwt_unix.Client.create_connection ~error_handler:ignore socket
  | _ -> assert false

let connection () =
  match !persistent_connection with
  | Some connection when H2_lwt_unix.Client.is_closed connection = false ->
      print_endline "Reusing existing connection.";
      Lwt.return connection
  | _ ->
      create_connection () >>= fun connection ->
      print_endline "Connection established.";
      persistent_connection := Some connection;
      Lwt.return connection

let do_grpc ~service ~rpc ~request ~decode ~pp =
  let f response =
    response >>= function
    | Some response ->
        Lwt.return
          (match decode response with
          | Ok value -> Ok (Format.asprintf "%a\n" pp value)
          | Error _ -> Error "decoding error")
    | None -> Lwt.return (Error "no response")
  in
  let handler = Grpc_lwt.Client.Rpc.unary ~f request in
  connection () >>= fun connection ->
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
    ~pp:PutResponse.pp

let get ~key =
  let request =
    RangeRequest.make ~key:(Bytes.of_string key) ()
    |> RangeRequest.to_proto |> Ocaml_protoc_plugin.Writer.contents
  in
  let decode response =
    Ocaml_protoc_plugin.Reader.create response |> RangeResponse.from_proto
  in
  do_grpc ~service:"etcdserverpb.KV" ~rpc:"Range" ~request ~decode
    ~pp:RangeResponse.pp

let server =
  let callback _conn req body =
    let key = req |> Request.uri |> Uri.path in
    let meth = req |> Request.meth |> Code.string_of_method in
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    (match meth with
    | "GET" -> get ~key
    | "POST" -> post ~key ~value:body
    | _ ->
        let message = "Only GET and POST are implemented!" in
        Lwt.return (Error (Grpc.Status.v ~message Unimplemented)))
    >>= function
    | Ok (Ok ret, status) ->
        Format.printf "Success status: %a@." Grpc.Status.pp status;
        Server.respond_string ~status:`OK ~body:ret ()
    | Ok (Error error, status) ->
        Format.printf "Status: %a@." Grpc.Status.pp status;
        Server.respond_string ~status:`Internal_server_error ~body:error ()
    | Error status ->
        Format.printf "Error status: %a@." Grpc.Status.pp status;
        let body =
          Option.value ~default:"No status message" (Grpc.Status.message status)
        in
        Server.respond_string ~status:`Internal_server_error ~body ()
  in
  Server.create ~mode:(`TCP (`Port cohttp_port)) (Server.make ~callback ())

let () = Lwt_main.run server
