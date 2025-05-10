open Routeguide_proto
module Pb = Route_guide

let random_point () =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Pb.default_point ~latitude ~longitude ()

let get_feature sw io =
  let request =
    Pb.default_point ~latitude:409146138 ~longitude:(-746188906) ()
  in

  Printf.printf "[UNARY] Getting a feature...\n%!";
  match RouteGuideClient.Result.get_feature ~sw ~io request with
  | Error _ -> Printf.printf "[UNARY] Error getting a feature\n%!"
  | Ok { response = feature; _ } ->
      Format.printf "[UNARY] Received a feature: %a@." Pb.pp_feature feature

let list_features sw io =
  let rectangle =
    Pb.default_rectangle
      ~lo:
        (Some (Pb.default_point ~latitude:400000000 ~longitude:(-750000000) ()))
      ~hi:
        (Some (Pb.default_point ~latitude:420000000 ~longitude:(-730000000) ()))
      ()
  in

  let handler : _ -> Pb.feature Seq.t -> unit =
   fun _h2_response seq ->
    Seq.iter
      (fun feature ->
        Format.printf "[S_STREAMING] Received a feature: %a@." Pb.pp_feature
          feature)
      seq
  in

  Printf.printf "[S_STREAMING] Listing features...\n%!";
  match RouteGuideClient.Result.list_features ~sw ~io rectangle handler with
  | Error _ -> Printf.printf "[S_STREAMING] Error listing features\n%!"
  | Ok _ -> ()

let run_record_route sw io clock =
  let points = List.to_seq @@ List.init 10 (fun _ -> random_point ()) in

  let handler : _ -> writer:(Pb.point -> bool) -> unit =
   fun _h2_response ~writer ->
    Seq.iter
      (fun point ->
        writer point |> ignore;
        Format.printf "[C_STREAMING] Sent point: %a@." Pb.pp_point point;
        Eio.Time.sleep clock 0.1)
      points
  in

  Printf.printf "[C_STREAMING] Sending points...\n%!";
  match RouteGuideClient.Result.record_route ~sw ~io handler with
  | Error _ -> Printf.printf "[C_STREAMING] Error listing features\n%!"
  | Ok _ -> ()

let run_route_chat sw io clock =
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

  let handler :
      _ -> writer:(Pb.route_note -> bool) -> read:Pb.route_note Seq.t -> unit =
   fun _h2_response ~writer ~read ->
    let rec loop notes read =
      match notes () with
      | Seq.Nil -> ()
      | Cons (note, rest_notes) -> (
          writer note |> ignore;
          Format.printf "[BI_STREAMING] Sent a note: %a@." Pb.pp_route_note note;

          Eio.Time.sleep clock 0.2;

          match read () with
          | Seq.Nil -> failwith "Expecting response"
          | Cons (note, rest_read) ->
              Format.printf "[BI_STREAMING] Received a note: %a@."
                Pb.pp_route_note note;

              (loop [@tailcall]) rest_notes rest_read)
    in
    loop route_notes read
  in

  Printf.printf "[BI_STREAMING] Exchanging notes...\n%!";
  match RouteGuideClient.Result.route_chat ~sw ~io handler with
  | Error _ -> Printf.printf "[BI_STREAMING] Error listing features\n%!"
  | Ok _ -> ()

let main env =
  let network = Eio.Stdenv.net env in
  let () = Random.self_init () in

  let run sw =
    let io, disconnect =
      Haha_client_io.create ~debug:true ~net:network ~sw "http://localhost:8080"
    in

    Printf.printf "*** SIMPLE RPC ***\n%!";

    get_feature sw io;

    Printf.printf "\n*** SERVER STREAMING ***\n%!";
    list_features sw io;

    Printf.printf "\n*** CLIENT STREAMING ***\n%!";
    run_record_route sw io env#clock;

    Printf.printf "\n*** BIDIRECTIONAL STREAMING ***\n%!";
    run_route_chat sw io env#clock;

    Printf.printf "Disconnecting\n%!";
    disconnect ()
  in

  Eio.Switch.run run

let () = Eio_main.run main
