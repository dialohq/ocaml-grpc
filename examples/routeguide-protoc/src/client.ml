open Grpc_eio
module Route_guide = Routeguide_protoc.Route_guide

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
  let response =
    Client.Typed_rpc.call
      (Grpc_protoc.Client_rpc.unary Route_guide.RouteGuide.Client.getFeature)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.unary point ~f:(function
          | Some feature -> feature
          | None -> Route_guide.default_feature ()))
      ()
  in
  match response with
  | Ok (res, _ok) ->
      Format.printf "RESPONSE = {%s}" (Route_guide.show_feature res)
  | Error _ -> Printf.printf "an error occurred"

(* $MDX part-end *)
(* $MDX part-begin=client-list-features *)
let print_features connection =
  let rectangle =
    Route_guide.default_rectangle
      ~lo:
        (Routeguide_protoc.Route_guide.default_point ~latitude:400000000l
           ~longitude:(-750000000l) ()
        |> Option.some)
      ~hi:
        (Routeguide_protoc.Route_guide.default_point ~latitude:420000000l
           ~longitude:(-730000000l) ()
        |> Option.some)
      ()
  in

  let stream =
    Client.Typed_rpc.call
      (Grpc_protoc.Client_rpc.server_streaming
         Route_guide.RouteGuide.Client.listFeatures)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:(Client.Typed_rpc.server_streaming rectangle ~f:Fun.id)
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Seq.iter
        (fun f -> Format.printf "RESPONSE = {%a}" Route_guide.pp_feature f)
        results
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))

(* $MDX part-end *)
(* $MDX part-begin=client-random-point *)
let random_point () : Route_guide.point =
  let latitude = (Random.int 180 - 90) * 10000000 |> Int32.of_int in
  let longitude = (Random.int 360 - 180) * 10000000 |> Int32.of_int in
  Route_guide.default_point ~latitude ~longitude ()

(* $MDX part-end *)
(* $MDX part-begin=client-record-route *)
let run_record_route connection =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let response =
    Client.Typed_rpc.call
      (Grpc_protoc.Client_rpc.client_streaming
         Route_guide.RouteGuide.Client.recordRoute)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.client_streaming ~f:(fun f response ->
             (* Stream points to server. *)
             Seq.iter (fun point -> Seq.write f point) points;

             (* Signal we have finished sending points. *)
             Seq.close_writer f;

             (* Decode RouteSummary responses. *)
             Eio.Promise.await response |> function
             | Some summary -> summary
             | None -> failwith (Printf.sprintf "No RouteSummary received.")))
      ()
  in
  match response with
  | Ok (result, _ok) ->
      Format.printf "SUMMARY = {%a}" Route_guide.pp_route_summary result
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
               ( Route_guide.default_route_note
                   ~location:(random_point () |> Option.some)
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
  in
  (* $MDX part-end *)
  (* $MDX part-begin=client-route-chat-2 *)
  let rec go writer reader notes =
    match Seq.uncons notes with
    | None ->
        Seq.close_writer writer (* Signal no more notes from the client. *)
    | Some (route_note, xs) -> (
        Seq.write writer route_note;

        (* Yield and sleep, waiting for server reply. *)
        Eio.Time.sleep clock 1.0;
        Eio.Fiber.yield ();

        match Seq.uncons reader with
        | None -> failwith "Expecting response"
        | Some (route_note, reader') ->
            Format.printf "NOTE = {%s}\n"
              (Route_guide.show_route_note route_note);
            go writer reader' xs)
  in
  let result =
    Client.Typed_rpc.call
      (Grpc_protoc.Client_rpc.bidirectional_streaming
         Route_guide.RouteGuide.Client.routeChat)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.bidirectional_streaming ~f:(fun writer reader ->
             go writer reader route_notes))
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
      Route_guide.default_point ~latitude:409146138l ~longitude:(-746188906l) ()
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
