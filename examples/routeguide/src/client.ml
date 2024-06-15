open Routeguide
module Client = Grpc_client_eio.Client

let get_feature sw io request =
  let response =
    Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"GetFeature"
      ~headers:(Grpc_client.make_request_headers `Proto) (fun encoder ->
        Route_guide.encode_pb_point request encoder)
  in
  match response with
  | `Success ({ response = res; _ } as result) ->
      `Success
        {
          result with
          response =
            res.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_feature;
        }
  | ( `Premature_close _ | `Write_error _ | `Connection_error _
    | `Response_not_ok _ ) as rest ->
      rest

(* $MDX part-end *)
(* $MDX part-begin=client-get-feature *)
let call_get_feature sw io point =
  let response =
    Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"GetFeature"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (fun encoder -> Route_guide.encode_pb_point point encoder)
  in
  match response with
  | `Success { response = res; _ } ->
      Printf.printf "RESPONSE = {%s}%!"
        (Route_guide.show_feature
           (res.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_feature))
  | _ -> Printf.printf "an error occurred"

(* $MDX part-end *)

(* $MDX part-begin=client-list-features *)
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
    Client.Server_streaming.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"ListFeatures"
      ~headers:(Grpc_client.make_request_headers `Proto)
      (Route_guide.encode_pb_rectangle rectangle) (fun _ ~read ->
        Seq.iter
          (fun f ->
            Printf.printf "RESPONSE = {%s}%!"
              (Route_guide.show_feature
                 (f.Grpc_eio_core.Body_reader.consume
                    Route_guide.decode_pb_feature)))
          read)
  in
  match stream with
  | `Stream_result { err = None; _ } -> ()
  | _ -> failwith "an erra"

(* $MDX part-end *)
(* $MDX part-begin=client-record-route *)
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
    Client.Client_streaming.call ~io ~sw ~service:"routeguide.RouteGuide"
      ~headers:(Grpc_client.make_request_headers `Proto)
      ~method_name:"RecordRoute" (fun _ ~writer ->
        Seq.iter
          (fun point ->
            writer.write (Route_guide.encode_pb_point point) |> ignore;
            Printf.printf "SENT = {%s}\n%!" (Route_guide.show_point point))
          points)
  in
  match response with
  | `Success { response; _ } ->
      Printf.printf "SUMMARY = {%s}\n%!"
        (Route_guide.show_route_summary
           (response.Grpc_eio_core.Body_reader.consume
              Route_guide.decode_pb_route_summary))
  | _ -> failwith "Error occured"

(* $MDX part-end *)
(* $MDX part-begin=client-route-chat-1 *)
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
  let rec go ~send ~close reader notes =
    match Seq.uncons notes with
    | None -> () (* Signal no more notes from the server. *)
    | Some (route_note, xs) -> (
        send (Route_guide.encode_pb_route_note route_note) |> ignore;

        Eio.Time.sleep clock 1.0;

        match reader () with
        | Seq.Nil -> failwith "Expecting response"
        | Seq.Cons (route_note, reader') ->
            Printf.printf "NOTE = {%s}\n%!"
              (Route_guide.show_route_note
                 (route_note.Grpc_eio_core.Body_reader.consume
                    Route_guide.decode_pb_route_note));
            go ~send ~close reader' xs)
  in
  let result =
    Client.Bidirectional_streaming.call ~service:"routeguide.RouteGuide"
      ~method_name:"RouteChat" ~io ~sw
      ~headers:(Grpc_client.make_request_headers `Proto) (fun _ ~writer ~read ->
        go ~send:writer.write ~close:writer.close read route_notes;
        [])
  in
  match result with
  | `Stream_result { err = None; _ } -> ()
  | _e -> failwith "Error"

(* $MDX part-end *)
(* $MDX part-end *)

(* $MDX part-begin=client-main *)

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

    let request =
      Route_guide.default_point ~latitude:409146138 ~longitude:(-746188906) ()
    in
    let result = call_get_feature sw io request in

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    print_features sw io;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw io;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat clock io sw;
    result
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)

let list_features ~sw ~io request handler =
  Client.Server_streaming.call ~sw ~io ~service:"routeguide.RouteGuide"
    ~method_name:"ListFeatures"
    ~headers:(Grpc_client.make_request_headers `Proto)
    (Route_guide.encode_pb_rectangle request) (fun net_response ~read ->
      let responses =
        Seq.map
          (fun response ->
            response.Grpc_eio_core.Body_reader.consume
              Route_guide.decode_pb_feature)
          read
      in
      handler net_response responses)
