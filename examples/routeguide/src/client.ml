open Grpc_lwt
open Lwt.Syntax
open Routeguide.Route_guide.Routeguide

(* TODO Can we derive these? *)
let point_to_s (point : Point.t) =
  Printf.sprintf "Point { latitude = %i; longitude = %i }" point.latitude point.longitude

let feature_to_s (feature : Feature.t) =
  Printf.sprintf "Feature { name = %s; point = %s}" feature.name (Option.map point_to_s feature.location |> Option.value ~default:"None")

let route_summary_to_s (route_summary : RouteSummary.t) =
  Printf.sprintf "RouteSummary { point_count = %i; feature_count = %i; distance = %i; elapsed_time = %i }"
    route_summary.point_count route_summary.feature_count route_summary.distance route_summary.elapsed_time

let route_note_to_s (route_note : RouteNote.t) =
  Printf.sprintf "RouteNote { location = %s; message = %s}" (Option.map point_to_s route_note.location |> Option.value ~default:"None") route_note.message

let client address port =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let call_get_feature connection point =
  let open Ocaml_protoc_plugin in
  let (encode, decode) = Service.make_client_functions RouteGuide.getFeature in
  let point_encoded = encode point |> Writer.contents in

  Client.call ~service:"routeguide.RouteGuide" ~rpc:"GetFeature"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary point_encoded ~f:(fun response ->
           let+ response = response in
           match response with
           | Some response ->
              (Reader.create response |> decode
               |> function | Ok feature -> feature
                           | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e)))
           | None -> Feature.make ()))
    ()

let print_features connection () : unit Lwt.t =
  let open Ocaml_protoc_plugin in
  let rectangle = Rectangle.make ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
                    ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ()) () in

  let (encode, decode) = Service.make_client_functions RouteGuide.listFeatures in
  let rectangle_encoded = encode rectangle |> Writer.contents in
  let* stream = Client.call ~service:"routeguide.RouteGuide" ~rpc:"ListFeatures"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
      (Client.Rpc.server_streaming rectangle_encoded ~f:(fun responses ->
           let stream = Lwt_stream.map (fun str ->
                            Reader.create str
                            |> decode
                            |> function
                              | Ok feature -> feature
                              | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e))) responses in
           Lwt_stream.to_list stream
         ))
      () in
  match stream with
  | Ok (results, _ok) -> Lwt_list.iter_s (fun f -> Lwt_io.printlf "RESPONSE = {%s}" (feature_to_s f)) results
  | Error e ->
     let () = failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e)) in
     Lwt.return ()

let random_point () : Point.t =
  let latitude = ((Random.int 180) - 90) * 10000000 in
  let longitude = ((Random.int 360) - 180) * 10000000 in
  Point.make ~latitude ~longitude ()

let run_record_route connection () : unit Lwt.t =
  let open Ocaml_protoc_plugin in
  let point_count = Random.int 100 in

  let points : Point.t list = Seq.unfold (fun x ->
                                  match x with
                                  | 0 -> None
                                  | x -> Some (random_point (), x - 1)) point_count |> List.of_seq in

  let (encode, decode) = Service.make_client_functions RouteGuide.recordRoute in
  let* response = Client.call ~service:"routeguide.RouteGuide" ~rpc:"RecordRoute"
        ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
        ~handler:(Client.Rpc.client_streaming ~f:(fun f response ->
                      (* Stream points to server. *)
                      let* () = Lwt_list.iter_s (fun point -> Lwt.return (encode point |> Writer.contents |> fun x -> f (Some x))) points in
                      f None; (* Signal we have finished sending points. *)

                      (* Decode RouteSummary response. *)
                      response
                      |> Lwt.map @@
                           function
                           | Some str ->
                              (Reader.create str
                               |> decode
                               |> function
                                 | Ok feature -> feature
                                 | Error err -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error err)))
                           | None -> failwith (Printf.sprintf "No RouteSummary received.")
                    )
        ) ()
  in
  match response with
  | Ok (result, _ok) -> Lwt_io.printlf "SUMMARY = {%s}" (route_summary_to_s result)
  | Error e ->
     let () = failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e)) in
     Lwt.return ()

let run_route_chat connection () : unit Lwt.t =
  let open Ocaml_protoc_plugin in

  (* Generate locations. *)
  let location_count = 5 in
  let* () = Lwt_io.printf "Generating %i locations\n" location_count in
  let route_notes =
    location_count
    |> Seq.unfold (function
           | 0 -> None
           | x -> Some (RouteNote.make ~location:(random_point ()) ~message:(Printf.sprintf "Random Message %i" x) (), x - 1))
    |> List.of_seq in

  let (encode, decode) = Service.make_client_functions RouteGuide.routeChat in

  let rec go f stream notes =
    match notes with
    | [] -> f None; (* Signal no more notes from the client. *)
            Lwt.return ()
    | route_note::xs ->
       let () = encode route_note |> Writer.contents |> fun x -> f (Some x) in
       let* () = Lwt_unix.sleep 1.0 in (* Yield and sleep, waiting for server reply. *)

       let* response = Lwt_stream.next stream in
       let route_note = Reader.create response
                        |> decode
                        |> function
                          | Ok route_note -> route_note
                          | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e)) in
       let* () = Lwt_io.printf "NOTE = {%s}\n" (route_note_to_s route_note) in
       go f stream xs
  in
  let* result =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RouteChat"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:(Client.Rpc.bidirectional_streaming
                  ~f:(fun f stream -> go f stream route_notes)
      ) () in
  match result with
  | Ok ((), _ok) ->
     Lwt.return ()
  | Error e ->
     let () = failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e)) in
     Lwt.return ()

let () =
  let port = 8080 in
  let address = "localhost" in
  let () = Random.self_init () in

  Lwt_main.run
    (let* () = Lwt_io.printl "*** SIMPLE RPC ***" in
     let request = RouteGuide.GetFeature.Request.make ~latitude:(409146138) ~longitude: (-746188906) () in
     let* connection = client address port in
     let* response = call_get_feature connection request in
     let* () = match response with
       | Ok (res, _) -> Lwt_io.printlf "RESPONSE = {%s}" (feature_to_s res)
       | Error _ -> Lwt_io.printl "an error occurred"
     in
     let* () = Lwt_io.printl "\n*** SERVER STREAMING ***" in
     let* () = print_features connection () in

     let* () = Lwt_io.printl "\n*** CLIENT STREAMING ***" in
     let* () = run_record_route connection () in

     let* () = Lwt_io.printl "\n*** BIDIRECTIONAL STREAMING ***" in
     let* () = run_route_chat connection () in

     Lwt.return ())