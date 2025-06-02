open Routeguide_proto
module Pb = Route_guide

type feature_list = Pb.feature list [@@deriving of_yojson { exn = true }]

let in_range (point : Pb.point) (rect : Pb.rectangle) : bool =
  let lo = Option.get rect.lo in
  let hi = Option.get rect.hi in

  let left = Int.min lo.longitude hi.longitude in
  let right = Int.max lo.longitude hi.longitude in
  let top = Int.max lo.latitude hi.latitude in
  let bottom = Int.min lo.latitude hi.latitude in

  point.longitude >= left && point.longitude <= right
  && point.latitude >= bottom && point.latitude <= top

let pi = 4. *. atan 1.
let radians_of_degrees = ( *. ) (pi /. 180.)

let list_take n l =
  let[@tail_mod_cons] rec aux n l =
    match (n, l) with 0, _ | _, [] -> [] | n, x :: l -> x :: aux (n - 1) l
  in
  if n < 0 then invalid_arg "List.take";
  aux n l

let calc_distance (p1 : Pb.point) (p2 : Pb.point) : int =
  let cord_factor = 1e7 in
  let r = 6_371_000.0 in
  (* meters *)
  let lat1 = Float.of_int p1.latitude /. cord_factor in
  let lat2 = Float.of_int p2.latitude /. cord_factor in
  let lng1 = Float.of_int p1.longitude /. cord_factor in
  let lng2 = Float.of_int p2.longitude /. cord_factor in

  let lat_rad1 = radians_of_degrees lat1 in
  let lat_rad2 = radians_of_degrees lat2 in

  let delta_lat = radians_of_degrees (lat2 -. lat1) in
  let delta_lng = radians_of_degrees (lng2 -. lng1) in

  let a =
    (sin (delta_lat /. 2.0) *. sin (delta_lat /. 2.0))
    +. cos lat_rad1 *. cos lat_rad2
       *. sin (delta_lng /. 2.0)
       *. sin (delta_lng /. 2.0)
  in
  let c = 2.0 *. atan2 (sqrt a) (sqrt (1.0 -. a)) in
  Float.to_int (r *. c)

let get_server (features : feature_list) clock =
  let module RouteGuideServerImplementation = struct
    module GetFeature = struct
      let handler : Pb.point -> Pb.feature =
       fun point ->
        Printf.printf "[UNARY] /GetFeature\n%!";
        match
          List.find_opt
            (fun (f : Pb.feature) -> f.location = Some point)
            features
        with
        | Some feature ->
            Format.printf "[UNARY] Feature found: %a@." Pb.pp_feature feature;
            feature
        | None ->
            Format.printf "[UNARY] Feature not found.@.";
            { Pb.location = None; name = "" }
    end

    module ListFeatures = struct
      type context = Pb.feature Seq.t

      let initial_context = list_take 10 features |> List.to_seq

      let handler : Pb.rectangle -> context -> Pb.feature option * context =
       fun rectangle ->
        Printf.printf "[S_STREAMING] /ListFeatures\n%!";
        Format.printf "[S_STREAMING] Received a rectangle %a@." Pb.pp_rectangle
          rectangle;
        fun feature_seq ->
          Eio.Time.sleep clock 0.1;
          match feature_seq () with
          | Seq.Cons ((feature : Pb.feature), next) ->
              if in_range (Option.get feature.location) rectangle then (
                Format.printf "[S_STREAMING] Sending a feature: %a@."
                  Pb.pp_feature feature;
                (Some feature, next))
              else Eio.Fiber.await_cancel ()
          | Seq.Nil -> (None, feature_seq)
    end

    module RecordRoute = struct
      type context = int * int * int * Pb.point option * float

      let initial_context = (0, 0, 0, None, 0.)

      let reader : context -> Pb.point -> context =
        (* Printf.printf "[C_STREAMING] /RecordRoute\n%!"; *)
        let start = Eio.Time.now clock in
        fun (point_count, feature_count, distance, last_point, _) point ->
          Format.printf "[C_STREAMING] Received a point: %a@." Pb.pp_point point;

          let point_count = point_count + 1 in

          let feature_count =
            List.find_all
              (fun (feature : Pb.feature) -> feature.location = Some point)
              features
            |> fun x -> List.length x + feature_count
          in

          let distance =
            match last_point with
            | Some last_point -> calc_distance last_point point
            | None -> distance
          in

          (point_count, feature_count, distance, Some point, start)

      let respond : context -> Pb.route_summary =
       fun (point_count, feature_count, distance, _, start) ->
        Printf.printf "[C_STREAMING] Responding with summary...\n%!";
        {
          Pb.point_count;
          feature_count;
          distance;
          elapsed_time = Eio.Time.now clock -. start |> Float.to_int;
        }
    end

    module RouteChat = struct
      type context = [ `Empty | `Note of Pb.route_note | `End ]

      let initial_context = `Empty

      let reader : context -> Pb.route_note option -> context =
       fun buffer note ->
        match (buffer, note) with
        | `Empty, Some note ->
            Format.printf "[BI_STREAMING] Received a note: %a@."
              Pb.pp_route_note note;
            `Note note
        | _, None -> `End
        | _ -> Eio.Fiber.await_cancel ()

      let writer : context -> Pb.route_note option * context = function
        | `Empty -> Eio.Fiber.await_cancel ()
        | `Note note ->
            Format.printf "[BI_STREAMING] Sending a note: %a@." Pb.pp_route_note
              note;
            (Some note, `Empty)
        | `End -> (None, `End)
    end
  end in
  (module RouteGuideServerImplementation : Route_guide_server.Implementation)

let serve env addr connection_handler : unit =
  Eio.Switch.run @@ fun sw ->
  let server_socket =
    Eio.Net.listen env#net ~reuse_addr:true ~sw ~backlog:10 addr
  in
  let connection_handler a b =
    connection_handler a b;

    Printf.printf "Ending connection handler\n%!"
  in

  Eio.Net.run_server
    ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
    server_socket connection_handler

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Path to datafile required as the first argument."
  in

  let port = 8080 in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in

  let features = feature_list_of_yojson_exn (Yojson.Safe.from_file path) in

  Eio_main.run (fun env ->
      serve env addr
        (Route_guide_server.connection_handler (get_server features env#clock)))
