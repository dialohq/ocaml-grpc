open! Core
open! Async
open Grpc_async
open Routeguide_proto_async.Route_guide.Routeguide
open Ocaml_protoc_plugin

(* Derived data types to make reading JSON data easier. *)
type location = { latitude : int; longitude : int } [@@deriving yojson]
type feature = { location : location; name : string } [@@deriving yojson]
type feature_list = feature list [@@deriving yojson]

let features : Feature.t list ref = ref []

module RouteNotesMap = Hashtbl.Make (struct
  type t = Point.t

  let hash s = Hashtbl.hash s
  let t_of_sexp = Point.t_of_sexp
  let sexp_of_t = Point.sexp_of_t
  let compare = Point.compare
end)

(** Load route_guide data from a JSON file. *)
let load path : Feature.t list =
  let json = Yojson.Safe.from_file path in
  match feature_list_of_yojson json with
  | Ok v ->
      List.map
        ~f:(fun feature ->
          Feature.make ~name:feature.name
            ~location:
              (Point.make ~longitude:feature.location.longitude
                 ~latitude:feature.location.latitude ())
            ())
        v
  | Error err -> failwith err

let in_range (point : Point.t) (rect : Rectangle.t) : bool =
  let lo = Option.value_exn rect.lo in
  let hi = Option.value_exn rect.hi in

  let left = Int.min lo.longitude hi.longitude in
  let right = Int.max lo.longitude hi.longitude in
  let top = Int.max lo.latitude hi.latitude in
  let bottom = Int.min lo.latitude hi.latitude in

  point.longitude >= left && point.longitude <= right
  && point.latitude >= bottom && point.latitude <= top

let pi = 4. *. Float.atan 1.
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
    (Float.sin (delta_lat /. 2.0) *. Float.sin (delta_lat /. 2.0))
    +. Float.cos lat_rad1 *. Float.cos lat_rad2
       *. Float.sin (delta_lng /. 2.0)
       *. Float.sin (delta_lng /. 2.0)
  in
  let c = 2.0 *. Float.atan2 (sqrt a) (sqrt (1.0 -. a)) in
  Float.to_int (r *. c)

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
  printf "GetFeature = {:%s}" (Point.show point);

  (* Lookup the feature and if found return it. *)
  let feature =
    List.find
      ~f:(fun (f : Feature.t) ->
        match (f.location, point) with
        | Some p1, p2 -> Point.equal p1 p2
        | _, _ -> false)
      !features
  in
  printf "Found feature %s"
    (feature |> Option.map ~f:Feature.show |> Option.value ~default:"Missing");
  return
  @@
  match feature with
  | Some feature ->
      (Grpc.Status.(v OK), Some (feature |> encode |> Writer.contents))
  | None ->
      (* No feature was found, return an unnamed feature. *)
      ( Grpc.Status.(v OK),
        Some (Feature.make ~location:point () |> encode |> Writer.contents) )

let list_features (buffer : string) (pipe : string Pipe.Writer.t) :
    Grpc.Status.t Deferred.t =
  (* Decode request. *)
  printf "ListFeatures\n";

  let decode, encode = Service.make_service_functions RouteGuide.listFeatures in
  let rectangle =
    Reader.create buffer |> decode |> function
    | Ok v -> v
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in

  (* Lookup and reply with features found. *)
  printf "Lookup and reply with features found.\n";
  let _features =
    List.fold ~init:[] !features ~f:(fun accum (feature : Feature.t) ->
        if in_range (Option.value_exn feature.location) rectangle then (
          Pipe.write_without_pushback pipe (encode feature |> Writer.contents);
          feature :: accum)
        else accum)
  in
  return Grpc.Status.(v OK)

let record_route (stream : string Pipe.Reader.t) :
    (Grpc.Status.t * string option) Deferred.t =
  printf "RecordRoute\n";

  let last_point = ref None in
  let start = Unix.gettimeofday () in
  let decode, encode = Service.make_service_functions RouteGuide.recordRoute in

  let%bind point_count, feature_count, distance =
    Pipe.fold
      ~f:(fun (point_count, feature_count, distance) i ->
        let point =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        printf "  ==> Point = {%s}\n" (Point.show point);

        (* Increment the point count *)
        let point_count = point_count + 1 in

        (* Find features *)
        let feature_count, _ =
          List.fold_map !features ~init:feature_count
            ~f:(fun i (feature : Feature.t) ->
              if Point.equal (Option.value_exn feature.location) point then
                (i + 1, feature)
              else (i, feature))
        in

        (* Calculate the distance *)
        let distance =
          match !last_point with
          | Some last_point -> calc_distance last_point point
          | None -> distance
        in

        last_point := Some point;
        return (point_count, feature_count, distance))
      ~init:(0, 0, 0) stream
  in
  let stop = Unix.gettimeofday () in
  let elapsed_time = int_of_float (stop -. start) in
  let summary =
    RouteSummary.make ~point_count ~feature_count ~distance ~elapsed_time ()
  in
  printf "RecordRoute exit\n";
  return (Grpc.Status.(v OK), Some (encode summary |> Writer.contents))

let route_chat (stream : string Pipe.Reader.t) f =
  printf "RouteChat\n";

  let decode, encode = Service.make_service_functions RouteGuide.routeChat in
  let%bind () =
    Pipe.iter
      ~f:(fun i ->
        let note =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in
        printf "  ==> Note = {%s}\n" (RouteNote.show note);
        Pipe.write f (encode note |> Writer.contents))
      stream
  in

  printf "RouteChat exit\n";
  return Grpc.Status.(v OK)

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

let main port data_path () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to Tcp.Bind_to_address.Localhost
      (Tcp.Bind_to_port.On_port port)
  in
  features := load data_path;

  Tcp.(Server.create_sock ~on_handler_error:`Ignore where_to_listen)
    (H2_async.Server.create_connection_handler
       ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
       ~error_handler:(fun _ ?request:_ _ _ ->
         print_endline "an error occurred"))
  >>= fun _server -> Deferred.never ()

(* Runnable as: dune exec -- routeguide-server-async -data ./examples/routeguide/data/route_guide_db.json *)
let () =
  Command.async_spec ~summary:"Start a hello world async_ssl server"
    Command.Spec.(
      empty
      +> flag "-port"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on"
      +> flag "-data"
           (optional_with_default "./data" string)
           ~doc:"string path to datafile")
    main
  |> Command_unix.run
