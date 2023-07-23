open Grpc_eio
open Routeguide.Route_guide.Routeguide
open Ocaml_protoc_plugin

(* $MDX part-begin=client-h2 *)
let client ~sw host port network =
  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
  let socket = Eio.Net.connect ~sw network addr in
  H2_eio.Client.create_connection ~sw ~error_handler:ignore socket

(* $MDX part-end *)
(* $MDX part-begin=client-get-feature *)
let call_get_feature connection point =
  let encode, decode = Service.make_client_functions RouteGuide.getFeature in
  let response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"GetFeature"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.unary
           (encode point |> Writer.contents)
           ~f:(fun response ->
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
  | Ok (res, _ok) -> Printf.printf "RESPONSE = {%s}" (Feature.show res)
  | Error _ -> Printf.printf "an error occurred"

(* $MDX part-end *)
(* $MDX part-begin=client-list-features *)
let print_features connection =
  let rectangle =
    Rectangle.make
      ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
      ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ())
      ()
  in

  let encode, decode = Service.make_client_functions RouteGuide.listFeatures in
  let stream =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"ListFeatures"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.server_streaming
           (encode rectangle |> Writer.contents)
           ~f:(fun responses ->
             let stream =
               Seq.map
                 (fun str ->
                   Reader.create str |> decode |> function
                   | Ok feature -> feature
                   | Error e ->
                       failwith
                         (Printf.sprintf "Could not decode request: %s"
                            (Result.show_error e)))
                 (Grpc_eio.Stream.to_seq responses)
             in
             stream))
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Seq.iter
        (fun f -> Printf.printf "RESPONSE = {%s}" (Feature.show f))
        results
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-random-point *)
let random_point () : Point.t =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Point.make ~latitude ~longitude ()

(* $MDX part-end *)
(* $MDX part-begin=client-record-route *)
let run_record_route connection =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let encode, decode = Service.make_client_functions RouteGuide.recordRoute in
  let response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RecordRoute"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.client_streaming ~f:(fun ~send ~close response ->
             (* Stream points to server. *)
             Seq.iter
               (fun point -> encode point |> Writer.contents |> send)
               points;

             (* Signal we have finished sending points. *)
             close ();

             (* Decode RouteSummary responses. *)
             Eio.Promise.await response |> function
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
      Printf.printf "SUMMARY = {%s}" (RouteSummary.show result)
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-route-chat-1 *)
let run_route_chat clock connection =
  (* Generate locations. *)
  let location_count = 5 in
  Printf.printf "Generating %i locations\n" location_count;
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
  in
  (* $MDX part-end *)
  (* $MDX part-begin=client-route-chat-2 *)
  let encode, decode = Service.make_client_functions RouteGuide.routeChat in
  let rec go ~send ~close reader notes =
    match Seq.uncons notes with
    | None -> close () (* Signal no more notes from the client. *)
    | Some (route_note, xs) -> (
        encode route_note |> Writer.contents |> fun x ->
        send x;

        (* Yield and sleep, waiting for server reply. *)
        Eio.Time.sleep clock 1.0;
        Eio.Fiber.yield ();

        match Seq.uncons reader with
        | None -> failwith "Expecting response"
        | Some (response, reader') ->
            let route_note =
              Reader.create response |> decode |> function
              | Ok route_note -> route_note
              | Error e ->
                  failwith
                    (Printf.sprintf "Could not decode request: %s"
                       (Result.show_error e))
            in
            Printf.printf "NOTE = {%s}\n" (RouteNote.show route_note);
            go ~send ~close reader' xs)
  in
  let result =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RouteChat"
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.bidirectional_streaming ~f:(fun ~send ~close reader ->
             go ~send ~close (Stream.to_seq reader) route_notes))
      ()
  in
  match result with
  | Ok ((), _ok) -> ()
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-main *)

let main env =
  let port = "8080" in
  let host = "localhost" in
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let connection = client ~sw host port network in

    Printf.printf "*** SIMPLE RPC ***\n";
    let request =
      RouteGuide.GetFeature.Request.make ~latitude:409146138
        ~longitude:(-746188906) ()
    in
    let result = call_get_feature connection request in

    Printf.printf "\n*** SERVER STREAMING ***\n";
    print_features connection;

    Printf.printf "\n*** CLIENT STREAMING ***\n";
    run_record_route connection;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n";
    run_route_chat clock connection;

    Eio.Promise.await (H2_eio.Client.shutdown connection);
    result
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)
