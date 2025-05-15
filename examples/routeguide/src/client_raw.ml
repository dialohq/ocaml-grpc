open Routeguide_proto
open Grpc_client_eio

let random_point () =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Route_guide.default_point ~latitude ~longitude ()

let get_feature (type headers net_response stream_error connection_error) sw
    (io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) request =
  let response =
    Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"GetFeature"
      ~headers:(Grpc_client.make_request_headers `Proto) (fun encoder ->
        Route_guide.encode_pb_point request encoder)
  in
  let (module Io') = io in
  match response with
  | `Success ({ response = res; _ } as result) ->
      {
        result with
        response =
          res.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_feature;
      }
  | #Rpc_error.Unary.error' as rest -> Io'.raise_client_error (Unary rest)

let _ = get_feature

let run_record_route (type headers net_response stream_error connection_error)
    sw
    (io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) =
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
  | `Success resp -> resp
  | #Rpc_error.Client_streaming.error' as rest ->
      let (module Io') = io in
      Io'.raise_client_error (Client_streaming rest)

let print_features (type headers net_response stream_error connection_error) sw
    (io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) =
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
  | `Stream_result_success result -> result
  | #Rpc_error.Server_streaming.error' as rest ->
      let (module Io') = io in
      Io'.raise_client_error (Server_streaming rest)

let run_route_chat (type headers net_response stream_error connection_error)
    clock
    (io :
      ( headers,
        net_response,
        Pbrt.Encoder.t -> unit,
        Pbrt.Decoder.t Grpc_eio_core.Body_reader.consumer,
        stream_error,
        connection_error )
      Grpc_client_eio.Io.t) sw =
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
  | `Stream_result_success result -> result
  | #Rpc_error.Bidirectional_streaming.error' as rest ->
      let (module Io') = io in
      Io'.raise_client_error (Bidirectional_streaming rest)

module Expert = struct
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
    Client.Bidirectional_streaming.call ~service:"routeguide.RouteGuide"
      ~method_name:"RouteChat" ~io ~sw
      ~headers:(Grpc_client.make_request_headers `Proto) (fun _ ~writer ~read ->
        go ~send:writer.write ~close:writer.close read route_notes;
        [])

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

  let get_feature sw io request =
    Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
      ~method_name:"GetFeature"
      ~headers:(Grpc_client.make_request_headers `Proto) (fun encoder ->
        Route_guide.encode_pb_point request encoder)

  let _ = get_feature

  let run_record_route sw io =
    let points =
      Random.int 100
      |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
    in

    Client.Client_streaming.call ~io ~sw ~service:"routeguide.RouteGuide"
      ~headers:(Grpc_client.make_request_headers `Proto)
      ~method_name:"RecordRoute" (fun _ ~writer ->
        Seq.iter
          (fun point ->
            writer.write (Route_guide.encode_pb_point point) |> ignore;
            Printf.printf "SENT = {%s}\n%!" (Route_guide.show_point point))
          points)
end

module Result = struct
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
        ~headers:(Grpc_client.make_request_headers `Proto)
        (fun _ ~writer ~read ->
          go ~send:writer.write ~close:writer.close read route_notes;
          [])
    in
    match result with
    | `Stream_result_success result -> Ok result
    | #Rpc_error.Bidirectional_streaming.error' as rest -> Error rest

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
    | `Stream_result_success result -> Ok result
    | #Rpc_error.Server_streaming.error' as rest -> Error rest

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
    | `Success resp -> Ok resp
    | #Rpc_error.Client_streaming.error' as rest -> Error rest

  let get_feature sw io request =
    let response =
      Client.Unary.call ~sw ~io ~service:"routeguide.RouteGuide"
        ~method_name:"GetFeature"
        ~headers:(Grpc_client.make_request_headers `Proto) (fun encoder ->
          Route_guide.encode_pb_point request encoder)
    in
    match response with
    | `Success resp -> Ok resp
    | #Rpc_error.Unary.error' as rest -> Error rest

  let _ = get_feature
end

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
    get_feature sw io request |> ignore;
    Expert.get_feature sw io request |> ignore;
    Result.get_feature sw io request |> ignore;

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    print_features sw io |> ignore;
    Expert.print_features sw io |> ignore;
    Result.print_features sw io |> ignore;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw io |> ignore;
    Expert.run_record_route |> ignore;
    Result.run_record_route |> ignore;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat clock io sw |> ignore;
    Expert.run_route_chat clock io sw |> ignore;
    Result.run_route_chat clock io sw |> ignore
    (* disconnect () *)
  in

  Eio.Switch.run run

let () = Eio_main.run main

(* $MDX part-end *)

let _list_features ~sw ~io request handler =
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
