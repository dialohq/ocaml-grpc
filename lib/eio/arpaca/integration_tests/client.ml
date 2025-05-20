let print_features _sw channel =
  let rectangle =
    Route_guide.default_rectangle
      ~lo:
        (Some
           (Route_guide.default_point ~latitude:400000000
              ~longitude:(-750000000) ()))
      ~hi:
        (Some
           (Route_guide.default_point ~latitude:420000000
              ~longitude:(-730000000) ()))
      ()
  in

  let stream =
    RouteGuide_client.list_features ~channel rectangle (fun f ->
        Printf.printf "RESPONSE = {%s}%!" (Route_guide.show_feature f))
  in
  match stream with Ok _ -> () | _ -> failwith "an erra"

let random_point () =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Route_guide.default_point ~latitude ~longitude ()

let run_record_route _sw channel =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let points = Array.of_seq points in
  let i = ref 0 in

  let response =
    RouteGuide_client.record_route ~channel (fun () ->
        if !i < Array.length points then (
          let point = points.(!i) in
          Printf.printf "SENT = {%s}\n%!" (Route_guide.show_point point);
          incr i;
          Some point)
        else None)
  in
  match response with
  | Ok response ->
      Printf.printf "SUMMARY = {%s}\n%!"
        (Route_guide.show_route_summary response)
  | _ -> failwith "Error occured"

let run_route_chat clock channel =
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
                   ~location:(Some (random_point ()))
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
  in
  let route_notes = Array.of_seq route_notes in
  let i = ref 0 in

  (* $MDX part-end *)
  (* $MDX part-begin=client-route-chat-2 *)
  let writer : unit -> Route_guide.route_note option =
   fun () ->
    if !i < Array.length route_notes then (
      let note = route_notes.(!i) in
      Eio.Time.sleep clock 1.0;
      incr i;
      Some note)
    else None
  in

  let reader : Route_guide.route_note -> unit =
   fun note ->
    Printf.printf "NOTE = {%s}\n%!" (Route_guide.show_route_note note)
  in
  let result = RouteGuide_client.route_chat ~channel writer reader in
  match result with Ok _ -> () | _e -> failwith "Error"

let main env =
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let channel =
      Grpc_client_eio.Channel.create ~sw ~net:network "http://localhost:8080"
    in

    Printf.printf "*** SIMPLE RPC ***\n%!";

    let result =
      RouteGuide_client.get_feature ~channel
        (Route_guide.default_point ~latitude:409146138 ~longitude:(-746188906)
           ())
    in
    Printf.printf "RESPONSE = {%s}\n%!"
      (match result with
      | Ok response -> Route_guide.show_feature response
      | _ -> failwith "Error occured");

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    print_features sw channel;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw channel;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat clock channel
  in

  Eio.Switch.run run

let () = Eio_main.run main
