open Routeguide
module Server = Grpc_server_eio
module R = Route_guide

(* Derived data types to make reading JSON data easier. *)
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

(* $MDX part-begin=server-get-feature *)
let get_feature =
  Grpc_server_eio.Rpc.unary (fun req ->
      let point =
        req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_point
      in
      Format.printf "%a" Route_guide.pp_point point;
      Eio.traceln "GetFeature = {:%s}" (R.show_point point);
      let feature =
        List.find_opt (fun (f : feature) -> f.location = point) !features
        |> Option.map (fun { location; name } : R.feature ->
               { R.name; location = Some location })
      in
      Eio.traceln "Found feature %s"
        (feature |> Option.map R.show_feature |> Option.value ~default:"Missing");
      match feature with
      | Some feature ->
          ((fun encoder -> R.encode_pb_feature feature encoder), [])
      | None ->
          (* No feature was found, return an unnamed feature. *)
          (R.encode_pb_feature (R.default_feature ~location:(Some point) ()), []))

(* $MDX part-end *)
(* $MDX part-begin=server-grpc *)

let mk_handler f =
  { Grpc_server_eio.Rpc.headers = (fun _ -> Grpc_server.headers `Proto); f }

(*
let route_guide_service clock =
  let add_rpc = Server.Service.add_rpc in
  let open Server.Rpc in
  Server.Service.v ()
  |> add_rpc ~name:"GetFeature" ~rpc:(mk_handler (unary get_feature))
  |> add_rpc ~name:"ListFeatures"
       ~rpc:(mk_handler (server_streaming list_features))
  |> add_rpc ~name:"RecordRoute"
       ~rpc:(mk_handler (client_streaming (record_route clock)))
  |> add_rpc ~name:"RouteChat" ~rpc:(mk_handler route_chat)

let server clock =
  Server.(
    make ()
    |> add_service ~name:"routeguide.RouteGuide"
         ~service:(route_guide_service clock))
*)

(* $MDX part-end *)

let list_features =
  Grpc_server_eio.Rpc.server_streaming (fun req write ->
      let rectangle =
        req.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_rectangle
      in

      let () =
        List.iter
          (fun feature ->
            if in_range feature.location rectangle then
              write (fun encoder ->
                  R.encode_pb_feature
                    { R.location = Some feature.location; name = feature.name }
                    encoder)
            else ())
          !features
      in
      [])

(* $MDX part-end *)
(* $MDX part-begin=server-route-chat *)
let route_chat =
 fun read write ->
  Printf.printf "RouteChat\n%!";

  Seq.iter
    (fun i ->
      let note =
        i.Grpc_eio_core.Body_reader.consume Route_guide.decode_pb_route_note
      in
      Printf.printf "  ==> Note = {%s}\n%!" (Route_guide.show_route_note note);
      write (Route_guide.encode_pb_route_note note))
    read;
  Printf.printf "RouteChat exit\n%!";
  []

(* $MDX part-end *)
(* $MDX part-begin=server-record-route *)

(* $MDX part-end *)
(* $MDX part-begin=server-record-route *)
(*
let record_route (clock : _ Eio.Time.clock) stream =
  Eio.traceln "RecordRoute";
  let last_point = ref None in
  let start = Eio.Time.now clock in

  let point_count, feature_count, distance =
    Seq.fold_left
      (fun (point_count, feature_count, distance) i ->
        let point =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        Eio.traceln "  ==> Point = {%s}" (Point.show point);

        (* Increment the point count *)
        let point_count = point_count + 1 in

        (* Find features *)
        let feature_count =
          List.find_all
            (fun (feature : Feature.t) ->
              Point.equal (Option.get feature.location) point)
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
      (0, 0, 0)
      (Grpc_core_eio.Stream.to_seq stream)
  in
  let stop = Eio.Time.now clock in
  let elapsed_time = int_of_float (stop -. start) in
  let summary =
    RouteSummary.make ~point_count ~feature_count ~distance ~elapsed_time ()
  in
  Eio.traceln "RecordRoute exit\n";
  (Grpc_server.trailers_with_code OK, Some (encode summary |> Writer.contents))
  *)

let record_route clock =
  Grpc_server_eio.Rpc.client_streaming (fun stream ->
      Eio.traceln "RecordRoute";
      let last_point = ref None in
      let start = Eio.Time.now clock in

      let point_count, feature_count, distance =
        Seq.fold_left
          (fun (point_count, feature_count, distance) point ->
            let point =
              point.Grpc_eio_core.Body_reader.consume
                Route_guide.decode_pb_point
            in
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
          (0, 0, 0) stream
      in
      ( Route_guide.encode_pb_route_summary
          {
            point_count;
            feature_count;
            distance;
            elapsed_time = Eio.Time.now clock -. start |> Float.to_int;
          },
        [] ))

(* $MDX part-end *)

let server clock ~service ~meth =
  match (service, meth) with
  | "routeguide.RouteGuide", "GetFeature" -> mk_handler get_feature
  | "routeguide.RouteGuide", "ListFeatures" -> mk_handler list_features
  | "routeguide.RouteGuide", "RecordRoute" -> mk_handler (record_route clock)
  | "routeguide.RouteGuide", "RouteChat" -> mk_handler route_chat
  | _ ->
      raise (Grpc_server_eio.Server_error (Grpc.Status.make Unimplemented, []))

(* $MDX part-begin=server-main *)
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

  Eio_main.run (fun env -> serve (server (Eio.Stdenv.clock env)) env)
(* $MDX part-end *)
