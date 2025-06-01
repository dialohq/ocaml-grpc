open Routeguide_proto
module Pb = Route_guide

let random_point () =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Pb.default_point ~latitude ~longitude ()

let get_feature channel =
  let request =
    Pb.default_point ~latitude:409146138 ~longitude:(-746188906) ()
  in

  Printf.printf "[UNARY] Getting a feature...\n%!";
  match Route_guide_client.get_feature ~channel request with
  | Error status ->
      Format.printf "[UNARY] Error getting a feature: %a@." Grpc.Status.pp
        status
  | Ok feature ->
      Format.printf "[UNARY] Done. Received a feature: %a@." Pb.pp_feature
        feature

let list_features channel =
  let rectangle =
    Pb.default_rectangle
      ~lo:
        (Some (Pb.default_point ~latitude:400000000 ~longitude:(-750000000) ()))
      ~hi:
        (Some (Pb.default_point ~latitude:420000000 ~longitude:(-730000000) ()))
      ()
  in

  let handler : int -> Pb.feature -> int =
   fun context feature ->
    Format.printf "[S_STREAMING] Received a feature %i: %a@." context
      Pb.pp_feature feature;
    context + 1
  in

  Printf.printf "[S_STREAMING] Listing features...\n%!";
  match
    Route_guide_client.Expert.list_features ~channel ~initial_context:1
      rectangle handler
  with
  | Error status ->
      Format.printf "[S_STREAMING] Error listing features: %a@." Grpc.Status.pp
        status
  | Ok final_context ->
      Printf.printf "[S_STREAMING] Done. Final count: %i\n%!" final_context

let run_record_route channel clock =
  let points = Array.init 10 (fun _ -> random_point ()) in
  let i = ref 0 in

  let handler : int -> Pb.point option * int =
   fun context ->
    if !i < Array.length points then (
      let point = points.(!i) in
      Eio.Time.sleep clock 0.1;
      Format.printf "[C_STREAMING] Sending point %i: %a@." context Pb.pp_point
        point;
      incr i;
      (Some point, context + 1))
    else (None, context + 1)
  in

  Printf.printf "[C_STREAMING] Sending points...\n%!";
  match
    Route_guide_client.Expert.record_route ~channel ~initial_context:1 handler
  with
  | Error status ->
      Format.printf "[C_STREAMING] Error recording route: %a@." Grpc.Status.pp
        status
  | Ok (resp, final_context) ->
      Format.printf "[C_STREAMING] Received route summary: %a@."
        Pb.pp_route_summary resp;
      Printf.printf "[C_STREAMING] Done. Final count: %i\n%!" final_context

let run_route_chat channel clock =
  let location_count = 5 in
  let route_notes =
    location_count
    |> Seq.unfold (function
         | 0 -> None
         | x ->
             Some
               ( Pb.default_route_note
                   ~location:(Some (random_point ()))
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
  in

  let writer :
      int * int * Pb.route_note Seq.t ->
      Pb.route_note option * (int * int * Pb.route_note Seq.t) =
   fun (sent, received, next) ->
    match next () with
    | Seq.Cons (note, next) ->
        Eio.Time.sleep clock 0.1;
        Format.printf "[BI_STREAMING] Sent a note %i: %a@." sent
          Pb.pp_route_note note;
        (Some note, (sent + 1, received, next))
    | Nil -> (None, (sent, received, Seq.empty))
  in
  let reader :
      int * int * Pb.route_note Seq.t ->
      Pb.route_note ->
      int * int * Pb.route_note Seq.t =
   fun (sent, received, s) note ->
    Format.printf "[BI_STREAMING] Received a note %i: %a@." received
      Pb.pp_route_note note;
    (sent, received + 1, s)
  in
  (* let handler ~(writer : Pb.route_note option -> unit) *)
  (*     ~(reader : Pb.route_note Seq.t) = *)
  (*   let write () = *)
  (*     Seq.iter *)
  (*       (fun note -> *)
  (*         writer (Some note); *)
  (*         Format.printf "[BI_STREAMING] Sent a note: %a@." Pb.pp_route_note note; *)
  (*         Eio.Time.sleep clock 0.1) *)
  (*       route_notes; *)
  (*     writer None *)
  (*   in *)
  (*   let read () = *)
  (*     let rec loop_read read = *)
  (*       match read () with *)
  (*       | Seq.Nil -> () *)
  (*       | Cons (note, next) -> *)
  (*           Format.printf "[BI_STREAMING] Received a note: %a@." *)
  (*             Pb.pp_route_note note; *)
  (*           loop_read next *)
  (*     in *)
  (*     loop_read reader *)
  (*   in *)
  (*   Eio.Fiber.both write read *)
  (* in *)

  Printf.printf "[BI_STREAMING] Exchanging notes...\n%!";
  match
    Route_guide_client.Expert.route_chat ~initial_context:(1, 1, route_notes)
      ~channel writer reader
  with
  | Error status ->
      Format.printf "[BI_STREAMING] Error exchanging notes: %a@." Grpc.Status.pp
        status
  | Ok (final_sent, final_received, _) ->
      Printf.printf
        "[BI_STREAMING] Done. Final (sent, received) counts: (%i, %i)\n%!"
        final_sent final_received

let main env =
  Random.self_init ();

  let run sw =
    let channel =
      Grpc.Channel.create ~max_streams:5 ~sw ~net:env#net
        "http://127.0.0.1:8080"
    in

    Printf.printf "*** SIMPLE RPC ***\n%!";
    get_feature channel;
    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    list_features channel;
    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route channel env#clock;
    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat channel env#clock;
    Printf.printf "Disconnecting\n%!";
    Grpc.Channel.shutdown channel
  in

  Eio.Switch.run run

let () = Eio_main.run main
