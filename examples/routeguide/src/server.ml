open Grpc_lwt
open Routeguide.Route_guide.Routeguide
open Ocaml_protoc_plugin
open Lwt.Syntax

(* Derived data types to make reading JSON data easier. *)
type location = { latitude : int; longitude : int } [@@deriving yojson]
type feature = { location : location; name : string } [@@deriving yojson]
type feature_list = feature list [@@deriving yojson]

let features : Feature.t list ref = ref []

module RouteNotesMap = Hashtbl.Make (struct
  type t = Point.t

  let equal = Point.equal
  let hash s = Hashtbl.hash s
end)

(** Load route_guide data from a JSON file. *)
let load path : Feature.t list =
  let json = Yojson.Safe.from_file path in
  match feature_list_of_yojson json with
  | Ok v ->
      List.map
        (fun feature ->
          Feature.make ~name:feature.name
            ~location:
              (Point.make ~longitude:feature.location.longitude
                 ~latitude:feature.location.latitude ())
            ())
        v
  | Error err -> failwith err

let in_range (point : Point.t) (rect : Rectangle.t) : bool =
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
let calc_distance (p1 : Point.t) (p2 : Point.t) : int =
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
let get_feature buffer =
  let decode, encode = Service.make_service_functions RouteGuide.getFeature in
  (* Decode the request. *)
  let point =
    Reader.create buffer |> decode |> function
    | Ok v -> v
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  let* () = Lwt_io.printlf "GetFeature = {:%s}" (Point.show point) in

  (* Lookup the feature and if found return it. *)
  let feature =
    List.find_opt
      (fun (f : Feature.t) ->
        match (f.location, point) with
        | Some p1, p2 -> Point.equal p1 p2
        | _, _ -> false)
      !features
  in
  let* () =
    Lwt_io.printlf "Found feature %s"
      (feature |> Option.map Feature.show |> Option.value ~default:"Missing")
  in
  Lwt.return
  @@
  match feature with
  | Some feature ->
      (Grpc.Status.(v OK), Some (feature |> encode |> Writer.contents))
  | None ->
      (* No feature was found, return an unnamed feature. *)
      ( Grpc.Status.(v OK),
        Some (Feature.make ~location:point () |> encode |> Writer.contents) )

(* $MDX part-end *)
(* $MDX part-begin=server-list-features *)
let list_features (buffer : string) (f : string -> unit) : Grpc.Status.t Lwt.t =
  (* Decode request. *)
  let decode, encode = Service.make_service_functions RouteGuide.listFeatures in
  let rectangle =
    Reader.create buffer |> decode |> function
    | Ok v -> v
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in

  (* Lookup and reply with features found. *)
  let () =
    List.iter
      (fun (feature : Feature.t) ->
        if in_range (Option.get feature.location) rectangle then
          encode feature |> Writer.contents |> f
        else ())
      !features
  in
  Lwt.return Grpc.Status.(v OK)

(* $MDX part-end *)
(* $MDX part-begin=server-record-route *)
let record_route (stream : string Lwt_stream.t) =
  let* () = Lwt_io.printf "RecordRoute\n" in
  let* () = Lwt_io.(flush stdout) in

  let last_point = ref None in
  let start = Unix.gettimeofday () in
  let decode, encode = Service.make_service_functions RouteGuide.recordRoute in

  let* point_count, feature_count, distance =
    Lwt_stream.fold_s
      (fun i (point_count, feature_count, distance) ->
        let point =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        let* () = Lwt_io.printf "  ==> Point = {%s}\n" (Point.show point) in

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
        Lwt.return (point_count, feature_count, distance))
      stream (0, 0, 0)
  in
  let stop = Unix.gettimeofday () in
  let elapsed_time = int_of_float (stop -. start) in
  let summary =
    RouteSummary.make ~point_count ~feature_count ~distance ~elapsed_time ()
  in
  let* () = Lwt_io.printf "RecordRoute exit\n" in
  let* () = Lwt_io.(flush stdout) in
  Lwt.return (Grpc.Status.(v OK), Some (encode summary |> Writer.contents))

(* $MDX part-end *)
(* $MDX part-begin=server-route-chat *)
let route_chat (stream : string Lwt_stream.t) (f : string -> unit) =
  let* () = Lwt_io.printf "RouteChat\n" in
  let* () = Lwt_io.(flush stdout) in

  let decode, encode = Service.make_service_functions RouteGuide.routeChat in
  let* () =
    Lwt_stream.iter_s
      (fun i ->
        let note =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        let* () = Lwt_io.printf "  ==> Note = {%s}\n" (RouteNote.show note) in
        let* () = Lwt_io.(flush stdout) in
        Lwt.return (encode note |> Writer.contents |> f))
      stream
  in

  let* () = Lwt_io.printf "RouteChat exit\n" in
  let* () = Lwt_io.(flush stdout) in
  Lwt.return Grpc.Status.(v OK)

(* $MDX part-end *)
(* $MDX part-begin=server-grpc *)
let route_guide_service =
  Server.Service.(
    v ()
    |> add_rpc ~name:"GetFeature" ~rpc:(Unary get_feature)
    |> add_rpc ~name:"ListFeatures" ~rpc:(Server_streaming list_features)
    |> add_rpc ~name:"RecordRoute" ~rpc:(Client_streaming record_route)
    |> add_rpc ~name:"RouteChat" ~rpc:(Bidirectional_streaming route_chat)
    |> handle_request)

let server =
  Server.(
    v ()
    |> add_service ~name:"routeguide.RouteGuide" ~service:route_guide_service)

(* $MDX part-end *)
(* $MDX part-begin=server-main *)
let () =
  let port = 8080 in
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Path to datafile required."
  in

  (* Load features. *)
  features := load path;

  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let* _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      let* () = Lwt_io.printf "Listening on port %i for grpc requests\n" port in
      Lwt_io.(flush stdout));

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
(* $MDX part-end *)
