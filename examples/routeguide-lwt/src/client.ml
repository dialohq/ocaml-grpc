open Grpc_lwt
open Lwt.Syntax
open Routeguide.Route_guide.Routeguide
open Ocaml_protoc_plugin

let client address port : H2_lwt_unix.Client.t Lwt.t =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let call_get_feature connection point =
  let encode, decode = Service.make_client_functions RouteGuide.getFeature in
  let* response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"GetFeature"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.unary
           (encode point |> Writer.contents)
           ~f:(fun response ->
             let+ response = response in
             match response with
             | Some response -> (
                 Reader.create response |> decode |> function
                 | Ok feature -> feature
                 | Error e ->
                     failwith
                       (Printf.sprintf "Could not decode request: %s"
                          (Result.show_error e)))
             | None -> Feature.make ()))
      ()
  in
  match response with
  | Ok (res, _ok) -> Lwt_io.printlf "RESPONSE = {%s}" (Feature.show res)
  | Error _ -> Lwt_io.printl "an error occurred"

let print_features connection =
  let rectangle =
    Rectangle.make
      ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
      ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ())
      ()
  in

  let encode, decode = Service.make_client_functions RouteGuide.listFeatures in
  let* stream =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"ListFeatures"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.server_streaming
           (encode rectangle |> Writer.contents)
           ~f:(fun responses ->
             let stream =
               Lwt_stream.map
                 (fun str ->
                   Reader.create str |> decode |> function
                   | Ok feature -> feature
                   | Error e ->
                       failwith
                         (Printf.sprintf "Could not decode request: %s"
                            (Result.show_error e)))
                 responses
             in
             Lwt_stream.to_list stream))
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Lwt_list.iter_s
        (fun f -> Lwt_io.printlf "RESPONSE = {%s}" (Feature.show f))
        results
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

let random_point () : Point.t =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Point.make ~latitude ~longitude ()

let run_record_route connection =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
    |> List.of_seq
  in

  let encode, decode = Service.make_client_functions RouteGuide.recordRoute in
  let* response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RecordRoute"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.client_streaming ~f:(fun f response ->
             (* Stream points to server. *)
             let* () =
               Lwt_list.iter_s
                 (fun point ->
                   Lwt.return
                     (encode point |> Writer.contents |> fun x -> f (Some x)))
                 points
             in
             (* Signal we have finished sending points. *)
             f None;

             (* Decode RouteSummary responses. *)
             response
             |> Lwt.map @@ function
                | Some str -> (
                    Reader.create str |> decode |> function
                    | Ok feature -> feature
                    | Error err ->
                        failwith
                          (Printf.sprintf "Could not decode request: %s"
                             (Result.show_error err)))
                | None -> failwith (Printf.sprintf "No RouteSummary received.")))
      ()
  in
  match response with
  | Ok (result, _ok) ->
      Lwt_io.printlf "SUMMARY = {%s}" (RouteSummary.show result)
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

let run_route_chat connection =
  let location_count = 5 in
  let* () = Lwt_io.printf "Generating %i locations\n" location_count in
  let route_notes =
    location_count
    |> Seq.unfold (function
         | 0 -> None
         | x ->
             Some
               ( RouteNote.make ~location:(random_point ())
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
    |> List.of_seq
  in
  let encode, decode = Service.make_client_functions RouteGuide.routeChat in
  let rec go f stream notes =
    match notes with
    | [] ->
        f None;
        (* Signal no more notes from the client. *)
        Lwt.return ()
    | route_note :: xs ->
        let () = encode route_note |> Writer.contents |> fun x -> f (Some x) in

        (* Yield and sleep, waiting for server reply. *)
        let* () = Lwt_unix.sleep 1.0 in

        let* response = Lwt_stream.next stream in
        let route_note =
          Reader.create response |> decode |> function
          | Ok route_note -> route_note
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        let* () = Lwt_io.printf "NOTE = {%s}\n" (RouteNote.show route_note) in
        go f stream xs
  in
  let* result =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RouteChat"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.bidirectional_streaming ~f:(fun f stream ->
             go f stream route_notes))
      ()
  in
  match result with
  | Ok ((), _ok) -> Lwt.return ()
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

let () =
  let port = 8080 in
  let address = "localhost" in
  let () = Random.self_init () in

  Lwt_main.run
    (let* () = Lwt_io.printl "*** SIMPLE RPC ***" in
     let request =
       RouteGuide.GetFeature.Request.make ~latitude:409146138
         ~longitude:(-746188906) ()
     in
     let* connection = client address port in
     let* () = call_get_feature connection request in

     let* () = Lwt_io.printl "\n*** SERVER STREAMING ***" in
     let* () = print_features connection in

     let* () = Lwt_io.printl "\n*** CLIENT STREAMING ***" in
     let* () = run_record_route connection in

     let* () = Lwt_io.printl "\n*** BIDIRECTIONAL STREAMING ***" in
     let* () = run_route_chat connection in

     Lwt.return ())
