open Routeguide.Route_guide.Routeguide
open Ocaml_protoc_plugin
module Client = Grpc_client_eio.Client

(* $MDX part-end *)
(* $MDX part-begin=client-get-feature *)
let call_get_feature sw net point =
  let encode, decode = Service.make_client_functions RouteGuide.getFeature in
  let response =
    Client.unary ~sw
      ~headers:(Grpc_client.make_request_headers `Proto)
      ~service:"routeguide.RouteGuide" ~method_name:"GetFeature" ~net
      ~encode:(fun point -> encode point |> Writer.contents)
      ~decode:(fun str -> decode (Reader.create str))
      point
  in
  match response with
  | Ok res -> Printf.printf "RESPONSE = {%s}" (Feature.show res)
  | Error _ -> Printf.printf "an error occurred"

(* $MDX part-end *)
(* $MDX part-begin=client-list-features *)
let print_features sw net =
  let rectangle =
    Rectangle.make
      ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
      ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ())
      ()
  in

  let encode, decode = Service.make_client_functions RouteGuide.listFeatures in
  let stream =
    Client.server_streaming ~sw ~net ~service:"routeguide.RouteGuide"
      ~method_name:"ListFeatures"
      ~encode:(fun rectangle -> encode rectangle |> Writer.contents)
      ~decode:(fun str -> decode (Reader.create str))
      ~headers:(Grpc_client.make_request_headers `Proto)
      rectangle
      (fun ~read ->
        let rec loop () =
          match read () with
          | None -> ()
          | Some f ->
              Printf.printf "RESPONSE = {%s}" (Feature.show f);
              loop ()
        in
        loop ())
  in
  match stream with Ok () -> () | Error _e -> failwith "an erra"

(* $MDX part-end *)
(* $MDX part-begin=client-random-point *)
let random_point () : Point.t =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Point.make ~latitude ~longitude ()

(* $MDX part-end *)
(* $MDX part-begin=client-record-route *)
let run_record_route sw net =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let encode, decode = Service.make_client_functions RouteGuide.recordRoute in
  let response =
    Client.client_streaming ~net ~sw ~service:"routeguide.RouteGuide"
      ~method_name:"RecordRoute"
      ~encode:(fun point -> encode point |> Writer.contents)
      ~decode:(fun str -> decode (Reader.create str))
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun ~write -> Seq.iter write points)
  in
  match response with
  | Ok result -> Printf.printf "SUMMARY = {%s}" (RouteSummary.show result)
  | Error _e -> failwith "Error occured"

(* $MDX part-end *)
(* $MDX part-begin=client-route-chat-1 *)
let run_route_chat clock net sw =
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
    | None -> close () (* Signal no more notes from the server. *)
    | Some (route_note, xs) -> (
        send route_note;

        Eio.Time.sleep clock 1.0;

        match reader () with
        | None -> failwith "Expecting response"
        | Some route_note ->
            Printf.printf "NOTE = {%s}\n%!" (RouteNote.show route_note);
            go ~send ~close reader xs)
  in
  let result =
    Client.bidirectional_streaming ~service:"routeguide.RouteGuide"
      ~method_name:"RouteChat" ~net ~sw
      ~headers:(Grpc_client.make_request_headers `Proto)
      ~encode:(fun x -> encode x |> Writer.contents)
      ~decode:(fun x -> decode (Reader.create x))
      (fun ~writer ~take ->
        go ~send:writer.Client.write ~close:writer.close take route_notes)
  in
  match result with Ok () -> () | Error _e -> failwith "Error"

(* $MDX part-end *)
(* $MDX part-begin=client-main *)

let main env =
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let net =
      Grpc_eio_net_client_h2.create_client ~net:network ~sw
        "http://localhost:8080"
    in

    Printf.printf "*** SIMPLE RPC ***\n%!";
    let request =
      RouteGuide.GetFeature.Request.make ~latitude:409146138
        ~longitude:(-746188906) ()
    in
    let result = call_get_feature sw net request in

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    print_features sw net;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw net;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat clock net sw;

    result
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)
