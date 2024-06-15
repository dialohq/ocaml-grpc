module R = Route_guide

type location = R.point = { latitude : int; longitude : int }
[@@deriving yojson]

type feature = { name : string; location : location } [@@deriving yojson]
type feature_list = feature list [@@deriving yojson]

let features : feature list ref = ref []

module RouteNotesMap = Hashtbl.Make (struct
  type t = Route_guide.point

  let equal = ( = )
  let hash s = Hashtbl.hash s
end)

(** Load route_guide data from a JSON file. *)
let load path : feature list =
  let json = Yojson.Safe.from_file path in
  match feature_list_of_yojson json with Ok v -> v | Error err -> failwith err

let in_range (point : R.point) (rect : R.rectangle) : bool =
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

(* Calculates the distance between two points using the "haversine" formula. *)
(* This code was taken from http://www.movable-type.co.uk/scripts/latlong.html. *)
let calc_distance (p1 : R.point) (p2 : R.point) : int =
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

let serve server env : unit =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let connection_handler client_addr socket =
    Eio.Switch.run (fun sw ->
        Io_server_h2_ocaml_protoc.connection_handler ~sw server client_addr
          socket)
  in
  Eio.Net.run_server
    ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
    server_socket connection_handler

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Path to datafile required."
  in

  (* Load features. *)
  features := load path;
  Eio_main.run (fun env ->
      let module RouteGuideRpc : RouteGuide_server.Implementation = struct
        let get_feature _ point =
          Format.printf "%a" Route_guide.pp_point point;
          Eio.traceln "GetFeature = {:%s}" (R.show_point point);
          let feature =
            List.find_opt (fun (f : feature) -> f.location = point) !features
            |> Option.map (fun { location; name } : R.feature ->
                   { R.name; location = Some location })
          in
          Eio.traceln "Found feature %s"
            (feature |> Option.map R.show_feature
            |> Option.value ~default:"Missing");
          match feature with
          | Some feature -> (feature, [])
          | None ->
              (* No feature was found, return an unnamed feature. *)
              (R.default_feature ~location:(Some point) (), [])

        let list_features _ rectangle (write : R.feature -> unit) =
          List.iter
            (fun feature ->
              if in_range feature.location rectangle then
                write
                  { R.location = Some feature.location; name = feature.name }
              else ())
            !features;
          []

        let record_route _ read =
          let clock = Eio.Stdenv.clock env in
          Eio.traceln "RecordRoute";
          let last_point = ref None in
          let start = Eio.Time.now clock in

          let point_count, feature_count, distance =
            Seq.fold_left
              (fun (point_count, feature_count, distance) point ->
                Eio.traceln "  ==> Point = {%s}" (Route_guide.show_point point);

                (* Increment the point count *)
                let point_count = point_count + 1 in

                (* Find features *)
                let feature_count =
                  List.find_all
                    (fun (feature : feature) -> feature.location = point)
                    !features
                  |> fun x -> List.length x + feature_count
                in

                (* Calculate the distance *)
                let distance =
                  match !last_point with
                  | Some last_point -> calc_distance last_point point
                  | None -> distance
                in
                last_point := Some point;
                (point_count, feature_count, distance))
              (0, 0, 0) read
          in
          ( ({
               R.point_count;
               feature_count;
               distance;
               elapsed_time = Eio.Time.now clock -. start |> Float.to_int;
             }
              : R.route_summary),
            [] )

        let route_chat _ read write =
          Printf.printf "RouteChat\n%!";

          Seq.iter
            (fun note ->
              Printf.printf "  ==> Note = {%s}\n%!"
                (Route_guide.show_route_note note);
              write note)
            read;
          Printf.printf "RouteChat exit\n%!";
          []
      end in
      serve (RouteGuide_server.create_server (module RouteGuideRpc)) env)
