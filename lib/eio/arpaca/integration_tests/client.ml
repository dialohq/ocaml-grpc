let print_features sw io =
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
    RouteGuide_client.Expert.list_features ~sw ~io rectangle (fun _ read ->
        Seq.iter
          (fun f ->
            Printf.printf "RESPONSE = {%s}%!" (Route_guide.show_feature f))
          read)
  in
  match stream with `Stream_result_success _ -> () | _ -> failwith "an erra"

let random_point () =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Route_guide.default_point ~latitude ~longitude ()

let run_record_route sw io =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let response =
    RouteGuide_client.Expert.record_route ~io ~sw (fun _ ~writer ->
        Seq.iter
          (fun point ->
            writer point |> ignore;
            Printf.printf "SENT = {%s}\n%!" (Route_guide.show_point point))
          points)
  in
  match response with
  | `Success { response; _ } ->
      Printf.printf "SUMMARY = {%s}\n%!"
        (Route_guide.show_route_summary response)
  | _ -> failwith "Error occured"

let run_route_chat clock io sw =
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
  (* $MDX part-end *)
  (* $MDX part-begin=client-route-chat-2 *)
  let rec go ~send reader notes =
    match Seq.uncons notes with
    | None -> () (* Signal no more notes from the server. *)
    | Some (route_note, xs) -> (
        send route_note |> ignore;

        Eio.Time.sleep clock 1.0;

        match reader () with
        | Seq.Nil -> failwith "Expecting response"
        | Seq.Cons (route_note, reader') ->
            Printf.printf "NOTE = {%s}\n%!"
              (Route_guide.show_route_note route_note);
            go ~send reader' xs)
  in
  let result =
    RouteGuide_client.Expert.route_chat ~io ~sw (fun _ ~writer ~read ->
        go ~send:writer read route_notes;
        [])
  in
  match result with `Stream_result_success _ -> () | _e -> failwith "Error"

let main env =
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let io =
      Io_client_h2_ocaml_protoc.create_client ~net:network ~sw
        "http://localhost:8080"
    in

    Printf.printf "*** SIMPLE RPC ***\n%!";

    let result =
      RouteGuide_client.Expert.get_feature ~sw ~io
        (Route_guide.default_point ~latitude:409146138 ~longitude:(-746188906)
           ())
    in
    Printf.printf "RESPONSE = {%s}\n%!"
      (match result with
      | `Success { response; _ } -> Route_guide.show_feature response
      | _ -> failwith "Error occured");

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    print_features sw io;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw io;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat clock io sw
  in

  Eio.Switch.run run

let () = Eio_main.run main
