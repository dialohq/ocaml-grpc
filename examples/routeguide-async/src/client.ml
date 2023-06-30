open! Core
open! Async
open Grpc_async
open Routeguide_proto_async.Route_guide.Routeguide
open Ocaml_protoc_plugin

let client address port =
  let%bind socket =
    let%bind addresses =
      Unix.Addr_info.get ~host:address ~service:(Int.to_string port)
        [ Unix.Addr_info.AI_FAMILY Unix.PF_INET ]
    in
    let socket = Unix.Socket.create Unix.Socket.Type.tcp in
    let address =
      let sockaddr =
        match addresses with
        | hd :: _ -> hd.Unix.Addr_info.ai_addr
        | [] -> failwithf "call_server: no address for %s %d" address port ()
      in
      match sockaddr with
      | Unix.ADDR_INET (a, i) -> `Inet (a, i)
      | ADDR_UNIX u ->
          failwithf "can't make an Socket.Address.Inet out of a UNIX socket %s"
            u ()
    in
    Unix.Socket.connect socket address
  in
  let error_handler = function
    | `Invalid_response_body_length _resp ->
        printf "invalid response body length\n%!"
    | `Exn _exn -> printf "exception!\n%!"
    | `Malformed_response s -> printf "malformed response: %s\n%!" s
    | `Protocol_error (code, s) ->
        printf "protocol error: %s, %s\n" (H2.Error_code.to_string code) s
  in
  H2_async.Client.create_connection ~error_handler socket

let call_get_feature connection point =
  let encode, decode = Service.make_client_functions RouteGuide.getFeature in
  let%bind response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"GetFeature"
      ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.unary
           ~encoded_request:(encode point |> Writer.contents)
           ~handler:(function
             | Some response -> (
                 Reader.create response |> decode |> function
                 | Ok feature -> return feature
                 | Error e ->
                     failwith
                       (Printf.sprintf "Could not decode request: %s"
                          (Result.show_error e)))
             | None -> return (Feature.make ())))
      ()
  in
  match response with
  | Ok (res, _ok) ->
      printf "RESPONSE = {%s}" (Feature.show res);
      return ()
  | Error _ ->
      printf "an error occurred";
      return ()

let print_features connection =
  let rectangle =
    Rectangle.make
      ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
      ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ())
      ()
  in

  let encode, decode = Service.make_client_functions RouteGuide.listFeatures in

  let rec handler responses =
    match%bind Pipe.read responses with
    | `Ok str ->
        let () =
          Reader.create str |> decode |> function
          | Ok feature -> printf "RESPONSE = {%s}" (Feature.show feature)
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        handler responses
    | `Eof -> return ()
  in
  let%bind stream =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"ListFeatures"
      ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.server_streaming
           ~encoded_request:(encode rectangle |> Writer.contents)
           ~handler)
      ()
  in
  match stream with
  | Error e -> failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e))
  | Ok ((), _s) -> return ()

let random_point () : Point.t =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Point.make ~latitude ~longitude ()

let run_record_route connection =
  let points =
    Sequence.unfold ~init:(Random.int 100) ~f:(function
      | 0 -> None
      | x -> Some (random_point (), x - 1))
  in

  let encode, decode = Service.make_client_functions RouteGuide.recordRoute in
  let handler =
    Client.Rpc.client_streaming ~handler:(fun pipe_w response ->
        (* Stream points to server. *)
        let%bind () =
          Sequence.iter_m points ~return:Async.Deferred.return
            ~bind:Async.Deferred.bind ~f:(fun point ->
              encode point |> Writer.contents |> Pipe.write pipe_w)
        in
        (* Signal we have finished sending points. *)
        Async.Pipe.close pipe_w;

        match%bind response with
        | None -> failwith (Printf.sprintf "No RouteSummary received.")
        | Some str -> (
            Reader.create str |> decode |> function
            | Ok feature -> return feature
            | Error err ->
                failwith
                  (Printf.sprintf "Could not decode request: %s"
                     (Result.show_error err))))
  in

  let%bind response =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RecordRoute"
      ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
      ~handler ()
  in
  match response with
  | Ok (result, _ok) ->
      return (printf "SUMMARY = {%s}" (RouteSummary.show result))
  | Error e -> failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e))

let run_route_chat connection =
  (* Generate locations. *)
  let location_count = 5 in
  printf "Generating %i locations\n" location_count;
  let route_notes =
    Sequence.unfold ~init:location_count ~f:(function
      | 0 -> None
      | x ->
          Some
            ( RouteNote.make ~location:(random_point ())
                ~message:(Printf.sprintf "Random Message %i" x)
                (),
              x - 1 ))
  in
  let encode, decode = Service.make_client_functions RouteGuide.routeChat in
  let go writer reader notes =
    let%bind () =
      Sequence.iter_m ~bind:Deferred.bind ~return:Deferred.return
        ~f:(fun route_note ->
          printf "Sending NOTE = {%s}\n" (RouteNote.show route_note);
          let%bind () =
            Pipe.write writer (encode route_note |> Writer.contents)
          in

          (* Pause and wait for server reply. Not strictly necessary but it demonstrates that
             responses are streamed rather than arriving as one response.
          *)
          let%bind () = Async.after (Time_unix.Span.of_string "1s") in

          let%bind response = Pipe.read' reader in
          match response with
          | `Eof -> return (Pipe.close_read reader)
          | `Ok response ->
              Base.Queue.iter response ~f:(fun response ->
                  let route_note =
                    Reader.create response |> decode |> function
                    | Ok route_note ->
                        printf "Receiving NOTE = {%s}\n"
                          (RouteNote.show route_note);
                        route_note
                    | Error e ->
                        failwith
                          (Printf.sprintf "Could not decode request: %s"
                             (Result.show_error e))
                  in
                  printf "NOTE = {%s}\n" (RouteNote.show route_note));
              return ())
        notes
    in

    (* Signal no more notes from the client. *)
    return (Pipe.close writer)
  in
  let%bind result =
    Client.call ~service:"routeguide.RouteGuide" ~rpc:"RouteChat"
      ~do_request:(H2_async.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.bidirectional_streaming ~handler:(fun f stream ->
             go f stream route_notes))
      ()
  in
  match result with
  | Ok ((), _ok) -> return ()
  | Error e -> failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e))

let () =
  let port = 8080 in
  let address = "localhost" in
  let main () =
    printf "*** SIMPLE RPC ***";
    let request =
      RouteGuide.GetFeature.Request.make ~latitude:409146138
        ~longitude:(-746188906) ()
    in
    let%bind connection = client address port in
    let%bind () = call_get_feature connection request in

    printf "\n*** SERVER STREAMING ***";
    let%bind () = print_features connection in

    printf "\n*** CLIENT STREAMING ***\n";
    let%bind () = run_record_route connection in

    printf "\n*** BIDIRECTIONAL STREAMING ***\n";
    run_route_chat connection
  in

  (* Runnable as: dune exec -- routeguide-client-async *)
  let () = Random.self_init () in
  Command.async_spec ~summary:"Start a routeguide async client"
    Command.Spec.empty main
  |> Command_unix.run
