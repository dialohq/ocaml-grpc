open Routeguide_proto
module Pb = Route_guide

type feature_list = Pb.feature list [@@deriving of_yojson { exn = true }]
type trailers = (string * string) list

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
  let _ = clock in
  let module RouteGuideServerImplementation = struct
    (* underlaying protocol request (HTTP/2 in this case, and most cases) *)
    type net_request = Haha_server_io.Net_request.t

    let get_feature : net_request -> Pb.point -> Pb.feature * trailers =
     fun _ point ->
      Printf.printf "[UNARY] /GetFeature\n%!";

      match
        List.find_opt (fun (f : Pb.feature) -> f.location = Some point) features
      with
      | Some feature ->
          Format.printf "[UNARY] Feature found: %a@." Pb.pp_feature feature;
          (feature, [])
      | None ->
          Format.printf "[UNARY] Feature not found.";
          ({ Pb.location = None; name = "" }, [])

    let list_features :
        net_request -> Pb.rectangle -> (Pb.feature -> unit) -> trailers =
     fun _ rectangle write ->
      Printf.printf "[S_STREAMING] /ListFeatures\n%!";

      List.iter
        (fun (feature : Pb.feature) ->
          if in_range (Option.get feature.location) rectangle then (
            write feature;
            Format.printf "[S_STREAMING] Sent a feature: %a@." Pb.pp_feature
              feature)
          else ())
        features;
      []

    let record_route :
        net_request -> Pb.point Seq.t -> Pb.route_summary * trailers =
     fun _ read ->
      Printf.printf "[C_STREAMING] /RecordRoute\n%!";
      let start = Eio.Time.now clock in

      let point_count, feature_count, distance =
        let rec loop_seq :
            int * int * int ->
            Pb.point option ->
            Pb.point Seq.t ->
            int * int * int =
         fun (point_count, feature_count, distance) last_point read ->
          match read () with
          | Seq.Nil -> (point_count, feature_count, distance)
          | Cons (point, rest) ->
              Format.printf "[C_STREAMING] Received a point: %a@." Pb.pp_point
                point;

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
              loop_seq (point_count, feature_count, distance) (Some point) rest
        in
        loop_seq (0, 0, 0) None read
      in

      ( {
          Pb.point_count;
          feature_count;
          distance;
          elapsed_time = Eio.Time.now clock -. start |> Float.to_int;
        },
        [] )

    let route_chat :
        net_request ->
        Pb.route_note Seq.t ->
        (Pb.route_note -> unit) ->
        trailers =
     fun _ read write ->
      Printf.printf "[BI_STREAMING] /RouteChat\n%!";

      Seq.iter
        (fun (note : Pb.route_note) ->
          Format.printf "[BI_STREAMING] Received a note: %a@." Pb.pp_route_note
            note;
          write note;
          Format.printf "[BI_STREAMING] Sent the note back")
        read;

      Printf.printf "[BI_STREAMING] Done.\n%!";

      []
  end in
  (module RouteGuideServerImplementation : RouteGuideServer.Implementation
    with type net_request = Haha_server_io.Net_request.t)

let serve env addr server : unit =
  Eio.Switch.run @@ fun sw ->
  let server_socket = Eio.Net.listen env#net ~sw ~backlog:10 addr in
  let connection_handler = Haha_server_io.connection_handler ~sw server in

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
        (RouteGuideServer.create_server (get_server features env#clock)))
