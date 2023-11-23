# gRPC Basics: OCaml

This tutorial was adapted from the [grpc-go](https://github.com/grpc/grpc-go/blob/master/examples/gotutorial.md) tutorial. It provides a basic introduction to working with gRPC and OCaml. By working through this example you will learn how to:

 * Define a service in a `.proto` file
 * Generate server and client code
 * Write a simple client and server

It assumes you are familiar with [protocol buffers]() and basic OCaml. Note that the example in this tutorial uses the proto3 version of the protocol buffers language, you can find out more in the [proto3 language guide]().

[protocol buffers]: https://developers.google.com/protocol-buffers/docs/overview
[proto3 language guide]: https://protobuf.dev/programming-guides/proto3/

## Why use gRPC?

Our example is a simple route mapping application that lets clients get information about features on their route, create a summary of their route, and exchange route information such as traffic updates with the server and other clients.

With gRPC we can define our service once in a `.proto` file, and implement clients and servers in any of gRPC's supported languages, which in turn can be run in environments ranging from servers inside Google to your own tablet - all the complexity of communication between different languages and environments is handled for you by gRPC. We also get all the advantages of working with protocol buffers, including efficient serialization, a simple IDL, and easy interface updating.

## Prerequisites

To run the sample code and walk through this tutorial, the only prerequisite is OCaml itself. To get OCaml insalled follow the [Up and Running]() guide on ocaml.org.

[Up and Running]: https://ocaml.org/docs/up-and-running

## Running the example

Clone the OCaml gRPC repository:

<!-- $MDX skip -->
```shell
$ git clone https://github.com/dialohq/ocaml-grpc
$ cd ocaml-grpc
```

Run the server

<!-- $MDX skip -->
```shell
$ dune exec -- routeguide-server ./examples/routeguide/data/route_guide_db.json
```

In a separate shell, run the client

<!-- $MDX skip -->
```shell
$ dune exec -- routeguide-client
```

You should see some logging output in both terminal windows. On the shell where you ran the client binary, you should see the output of the bi-directional streaming rpc, printing 1 line per second.

<!-- $MDX skip -->
```shell
NOTE = RouteNote { location: Some(Point { latitude: 409146139, longitude: -746188906 }), message: "at 1.000319208s" }
```

If you scroll up you should see the output of the other three request types; simple rpc, server-side streaming, and client-side streaming.

## Project setup

For this tutorial, we will start by creating a new OCaml project with Dune:

<!-- $MDX skip -->
```shell
$ dune init project routeguide
$ cd routeguide
$ opam switch create . 5.0.0 --deps-only --with-test -y
```

`ocaml-grpc` works on OCaml `4.11` and above, the latest `5.0.0` version will give the best results. This tutorial uses EIO, a concurrent programming library for OCaml, other options exist like Async or LWT, example code for both exists in [examples]().

Our first step is to define the gRPC service and the method *request* and *response* types using [protocol buffers](). We will keep our `.proto` files in a directory in our project's root. There is no requirement where the `.proto` definitions live, [Dune]() will build them regardless.

<!-- $MDX skip -->
```shell
$ mkdir proto && touch proto/route_guide.proto
```

You can see the complete `.proto` file in [examples/routeguide/proto/route_guide.proto]().

To define a service, first we define a *service` in your *.proto* file:

<!-- $MDX skip -->
```protocol-buffer
service RouteGuide {
   ...
}
```

Then you define `rpc` methods inside this service definition, specifying their *request* and *response* types. gRPC lets you define four kinds of service methods, all of which will be used in the `RouteGuide` service:

 * A *simple RPC* where the client sends a request to the server and waits for a response to come back. Just like a normal function call.
<!-- $MDX skip -->
```protocol-buffer
  // Obtains the feature at a given position.
  rpc GetFeature(Point) returns (Feature) {}
```
 * A *server-side streaming RPC*  where the client sends a request to the server and gets a stream to read back a seqeuence of messages. The client reads from the returned stream until there are no more messages. In the example, it specifies a server-side streaming method by placing the `stream` keyword before the *response* type.
<!-- $MDX skip -->
```protocol-buffer
  // Obtains the Features available within the given Rectangle.  Results are
  // streamed rather than returned at once (e.g. in a response message with a
  // repeated field), as the rectangle may cover a large area and contain a
  // huge number of features.
  rpc ListFeatures(Rectangle) returns (stream Feature) {}
```

 * A *client-side streaming RPC*  where the client writes a sequence of messages and sends them to the server. Once the client has finished writing the messages, it waits for the server to read them all and return its response. To use client-side streaming,  the `stream` keword is placed before the *request* type.
<!-- $MDX skip -->
```protocol-buffer
  // Accepts a stream of Points on a route being traversed, returning a
  // RouteSummary when traversal is completed.
  rpc RecordRoute(stream Point) returns (RouteSummary) {}
```

 * A *bi-directional streaming RPC* where both sides send a sequence of messages. The two streams operate independently, so clients and servers can read and write in whatever order they like. For example, the server could wait to receive all the client messages before writing its responses, or it could alternately read a message then write a message, or some other combination of reads and writes. The order of messages in each steam is preserved. You specify this type of method by placing the `stream` keyword before both the *request* and *response* types.
<!-- $MDX skip -->
```protocol-buffer
  // Accepts a stream of RouteNotes sent while a route is being traversed,
  // while receiving other RouteNotes (e.g. from other users).
  rpc RouteChat(stream RouteNote) returns (stream RouteNote) {}
```

The `.proto` file also contains protocol buffer message type defintions for the *request* and *response* types used in the service methods. For example here is the `Point` message type:
<!-- $MDX skip -->
```protocol-buffer
// Points are represented as latitude-longitude pairs in the E7 representation
// (degrees multiplied by 10**7 and rounded to the nearest integer).
// Latitudes should be in the range +/- 90 degrees and longitude should be in
// the range +/- 180 degrees (inclusive).
message Point {
  int32 latitude = 1;
  int32 longitude = 2;
}
```

## Generating client and server code

OCaml gRPC can be configured to generate code as part of dune's normal build process. This is very convenient because once we've set everything up, there is no extra step to keep the generated code and our `.proto` definitions in sync.

OCaml gRPC provides a pluggable approach to protocol buffer serialisation and code generation. For this tutorial we use `ocaml-protoc-plugin` which has good coverage of the protobuf spec. Consult the [comparison](https://github.com/issuu/ocaml-protoc-plugin#comparison-with-other-ocaml-protobuf-handlers) with other OCaml protobuf implementations for more details.

Edit the `dune-project` to add `ocaml-protoc-plugin` as a dependency. Then add a `dune` file into the `proto` directory alongside the `route_guide.proto`.

<!-- $MDX include,file=routeguide/proto/dune -->
```ocaml
(library
 (name routeguide)
 (package grpc-examples)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.eq))
 (libraries ocaml-protoc-plugin))

(rule
 (targets route_guide.ml)
 (deps
  (:proto route_guide.proto))
 (action
  (run
   protoc
   -I
   .
   "--ocaml_out=annot=[@@deriving show { with_path = false }, eq]:."
   %{proto})))
```

<!-- $MDX skip -->
```shell
$ dune build proto
```

That's it. The generated code contains
 * Modules for the message types `Point`, `Rectangle`, `Feature`, `RouteNote` and `RouteSummary`.
 * A module for the `RouteGuide` service with the RPC operations defined as modules.

Don't worry about the generated API, we will cover it in detail as we implement `RouteGuide` service. If you are curious as to where the generated files are, keep reading. The mystery will be revealed soon! We can now move on to the fun part.

## Creating the server

First let's look at how we create a `RouteGuide` server. If you are only interested in creating gRPC clients, you can skip this section and go straight to [Creating the client]() (though you might find it interesting anyway!).

There are two parts to making out `RouteGuide` service do its job:
 * Implementing the service interface generated from our service definition, doing the actual work of our service.
 * Running a gRPC server to listen for requests from clients and dispatch them to the right service implementation.

You can find our example `RouteGuide` server in [examples/routeguide/src/server.ml](examples/routeguide/src/server.ml). Let's take a closer look at how it works.

### Implementing RouteGuide

As you can see, our server uses the `Service` module from `Grpc_eio` to build up a service implementation.
The individual service functions from our proto definition are implemented using `add_rpc` with matching names and rpc types, which must match the `route_guide.proto` definitions.

<!-- $MDX include,file=routeguide/src/server.ml,part=server-grpc -->
```ocaml
let server t clock =
  Server.Typed_rpc.server
    [ get_feature t; list_features t; record_route t clock; route_chat t ]
```

### Simple RPC

Let's look at the simplest type first, `GetFeature` which just gets a `Point` from the client and returns the corresponding feature information from its database in a `Feature`.

<!-- $MDX include,file=routeguide/src/server.ml,part=server-get-feature -->
```ocaml
let get_feature (t : t) =
  Grpc_eio.Server.Typed_rpc.unary
    (module RouteGuide.GetFeature)
    ~f:(fun point ->
      Eio.traceln "GetFeature = {:%s}" (Point.show point);

      (* Lookup the feature and if found return it. *)
      let feature =
        List.find_opt
          (fun (f : Feature.t) ->
            match (f.location, point) with
            | Some p1, p2 -> Point.equal p1 p2
            | _, _ -> false)
          t.features
      in
      Eio.traceln "Found feature %s"
        (feature |> Option.map Feature.show |> Option.value ~default:"Missing");
      match feature with
      | Some feature -> (Grpc.Status.(v OK), Some feature)
      | None ->
          (* No feature was found, return an unnamed feature. *)
          (Grpc.Status.(v OK), Some (Feature.make ~location:point ())))
```

The method is passed the client's `Point` protocol buffer request. It decodes the request into a `Point.t` and uses that to look up the feature. It returns a `Feature` protocol buffer object with the response information indicating the successful response, based on the feature found or an unnamed default feature.

### Server-side streaming RPC

Now let's look at one of our streaming RPCs. `list_features` is a server-side streaming RPC, so we need to send back multiple `Feature`s to our client.

<!-- $MDX include,file=routeguide/src/server.ml,part=server-list-features -->
```ocaml
let list_features (t : t) =
  Grpc_eio.Server.Typed_rpc.server_streaming
    (module RouteGuide.ListFeatures)
    ~f:(fun rectangle f ->
      (* Lookup and reply with features found. *)
      let () =
        List.iter
          (fun (feature : Feature.t) ->
            if in_range (Option.get feature.location) rectangle then f feature
            else ())
          t.features
      in
      Grpc.Status.(v OK))
```

Like `get_feature` `list_feature`'s input is a single message. A `Rectangle` that is decoded from a string buffer. The `f: (string -> unit)` function is for writing the encoded responses back to the client. In the function we decode the request, lookup any matching features and stream them back to the client as we find them using `f`. Once we've looked at all the `features` we respond with an `OK` indicating the streaming has finished successfully.

### Client-side streaming RPC

Now let's look at something a little more complicated: the client-side streaming function `RecordRoute`, where we get a stream of `Point`s from the client and return a single `RouteSummary` with information about their trip. As you can see, this time the method gets a `string Seq.t` representing the stream of points from the client. It decodes the stream of points, performs some calculations while accumulating the result, and finally responds with a route summary.

<!-- $MDX include,file=routeguide/src/server.ml,part=server-record-route -->
```ocaml
let record_route (t : t) (clock : _ Eio.Time.clock) =
  Grpc_eio.Server.Typed_rpc.client_streaming
    (module RouteGuide.RecordRoute)
    ~f:(fun (stream : Point.t Seq.t) ->
      Eio.traceln "RecordRoute";

      let last_point = ref None in
      let start = Eio.Time.now clock in

      let point_count, feature_count, distance =
        Seq.fold_left
          (fun (point_count, feature_count, distance) point ->
            Eio.traceln "  ==> Point = {%s}" (Point.show point);

            (* Increment the point count *)
            let point_count = point_count + 1 in

            (* Find features *)
            let feature_count =
              List.find_all
                (fun (feature : Feature.t) ->
                  Point.equal (Option.get feature.location) point)
                t.features
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
      let stop = Eio.Time.now clock in
      let elapsed_time = int_of_float (stop -. start) in
      let summary =
        RouteSummary.make ~point_count ~feature_count ~distance ~elapsed_time ()
      in
      Eio.traceln "RecordRoute exit\n";
      (Grpc.Status.(v OK), Some summary))
```

### Bidirectional streaming RPCs

Finally, let's look at our bidirectional streaming RPC `route_chat`, which receives a stream of `RouteNote`s and returns a stream of `RouteNote`s.

<!-- $MDX include,file=routeguide/src/server.ml,part=server-route-chat -->
```ocaml
let route_chat (_ : t) =
  Grpc_eio.Server.Typed_rpc.bidirectional_streaming
    (module RouteGuide.RouteChat)
    ~f:(fun (stream : RouteNote.t Seq.t) (f : RouteNote.t -> unit) ->
      Printf.printf "RouteChat\n";

      Seq.iter
        (fun note ->
          Printf.printf "  ==> Note = {%s}\n" (RouteNote.show note);
          f note)
        stream;

      Printf.printf "RouteChat exit\n";
      Grpc.Status.(v OK))
```

`route_chat` receives a `string Seq.t` of requests which it decodes, logs to stdout to show it has received the note, and then encodes again to send back to the client. Finally it responds with an `OK` indicating it has finished. The logic is we receive one `RouteNote` and respond directly with the same `RouteNote` using the `f` function supplied.

### Starting the server

Once we've implemented all our functions, we also need to startup a gRPC server so that clients can actually use our service. This is how our it looks:

<!-- $MDX include,file=routeguide/src/server.ml,part=server-main -->
```ocaml
let serve t env =
  let port = 8080 in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Eio.Switch.run @@ fun sw ->
  let handler = connection_handler ~sw (server t clock) in
  let server_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:10 addr
  in
  let rec listen () =
    Eio.Net.accept_fork ~sw server_socket
      ~on_error:(fun exn -> Eio.traceln "%s" (Printexc.to_string exn))
      handler;
    listen ()
  in
  Eio.traceln "Listening on port %i for grpc requests\n" port;
  listen ()

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Path to datafile required."
  in

  (* Load features. *)
  let t = { features = load_features path } in

  Eio_main.run (serve t)
```

To handle requests we use `h2-lwt-unix`, an implementation of the HTTP/2 specification entirely in OCaml. What that means is we can swap in other h2 implementations like MirageOS to run in a Unikernel or Async to use JaneStreet's alternatve async implementation. Furthermore we can add TLS or SSL encryptionon to our HTTP/2 stack. 

## Creating the client

In this section, we will look at creating a gRPC client for our `RouteGuide` service. You can see our complete example client code in [examples/routeguide/src/client.ml](./routeguide/src/client.ml).

To call service methods, we first need to create a H2 connection to communicate with the server. This connection will get reused by all calls to the server.

<!-- $MDX include,file=routeguide/src/client.ml,part=client-h2 -->
```ocaml
let client ~sw host port network =
  let inet, port =
    Eio_unix.run_in_systhread (fun () ->
        Unix.getaddrinfo host port [ Unix.(AI_FAMILY PF_INET) ])
    |> List.filter_map (fun (addr : Unix.addr_info) ->
           match addr.ai_addr with
           | Unix.ADDR_UNIX _ -> None
           | ADDR_INET (addr, port) -> Some (addr, port))
    |> List.hd
  in
  let addr = `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port) in
  let socket = Eio.Net.connect ~sw network addr in
  H2_eio.Client.create_connection ~sw ~error_handler:ignore socket
```

To call service methods, we take the H2 connection and build up a gRPC call for the service method using `Client.call` from the Client module.

### Simple RPC

Calling the simple RPC `get_feature` requires building up a `Client.call` representation that matches the proto defintion, filling in the labelled arguments `~service` and `~rpc` with the matching service implementations. `~do_request` gets the H2 function for writing to a Http/2 Body. The real work is done in the `~handler` function using `Client.Rpc.unary` to setup a Unary RPC response handler, which sends the encoded `Point` and calls the function `~f` with the response. Here we decode the response and log it to the console.

<!-- $MDX include,file=routeguide/src/client.ml,part=client-get-feature -->
```ocaml
let call_get_feature connection point =
  let response =
    Client.Typed_rpc.call
      (module RouteGuide.GetFeature)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.unary point ~f:(function
          | Some feature -> feature
          | None -> Feature.make ()))
      ()
  in
  match response with
  | Ok (res, _ok) -> Printf.printf "RESPONSE = {%s}" (Feature.show res)
  | Error _ -> Printf.printf "an error occurred"
```

### Server-side streaming RPC

Here we call the server-side streaming method `list_features`, which returns a stream of geographical `Feature`s.
<!-- $MDX include,file=routeguide/src/client.ml,part=client-list-features -->
```ocaml
let print_features connection =
  let rectangle =
    Rectangle.make
      ~lo:(Point.make ~latitude:400000000 ~longitude:(-750000000) ())
      ~hi:(Point.make ~latitude:420000000 ~longitude:(-730000000) ())
      ()
  in

  let stream =
    Client.Typed_rpc.call
      (module RouteGuide.ListFeatures)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:(Client.Typed_rpc.server_streaming rectangle ~f:Fun.id)
      ()
  in
  match stream with
  | Ok (results, _ok) ->
      Seq.iter
        (fun f -> Printf.printf "RESPONSE = {%s}" (Feature.show f))
        results
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))
```

As in the simple RPC we pass a single request value. However, instead of getting back a single value we get a stream of `Feature`s. We use `Seq.map` to iterate over the stream and decode each into a `Feature.t` and then print out the features when they are all decoded. Equally we could have printed the features as they are being decoded inside the `Seq.map` rather than gathering them all into a List and printing them at the end. Notice that the type signature for `Client.RPC.server_streaming` is similar to `unary` in that we provide an encoded request and provide a handler function to consume the response.

### Client-side streaming RPCs

The client-side streaming method `record_route` takes a stream of `Point`s and returns a single `RouteSummary` value.
<!-- $MDX include,file=routeguide/src/client.ml,part=client-random-point -->
```ocaml
let random_point () : Point.t =
  let latitude = (Random.int 180 - 90) * 10000000 in
  let longitude = (Random.int 360 - 180) * 10000000 in
  Point.make ~latitude ~longitude ()
```
We create a function for generating random points and then use that to generate a sequence of 100 points to record on our route.
<!-- $MDX include,file=routeguide/src/client.ml,part=client-record-route -->
```ocaml
let run_record_route connection =
  let points =
    Random.int 100
    |> Seq.unfold (function 0 -> None | x -> Some (random_point (), x - 1))
  in

  let response =
    Client.Typed_rpc.call
      (module RouteGuide.RecordRoute)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.client_streaming ~f:(fun f response ->
             (* Stream points to server. *)
             Seq.iter (fun point -> Seq.write f point) points;

             (* Signal we have finished sending points. *)
             Seq.close_writer f;

             (* Decode RouteSummary responses. *)
             Eio.Promise.await response |> function
             | Some summary -> summary
             | None -> failwith (Printf.sprintf "No RouteSummary received.")))
      ()
  in
  match response with
  | Ok (result, _ok) ->
      Printf.printf "SUMMARY = {%s}" (RouteSummary.show result)
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))
```

With this stream of points we setup another handler using `Client.Rpc.client_streaming`. The type of the callback arguments is important to understand, `f` is the function for sending data down the gRPC stream to the server. Calling it with `f (Some value)` will send the value to the server, while calling it with `f None` signals that we have finished streaming.  Here you can see we iterate over all the points and call `f` with Some value, and when we have sent everything we call `f None` to signal we are finished. Then we decode the `response` provided and print it out.

### Bidirectional streaming RPC

Finally, let's look at our bidirectional streaming RPC. THe `route_chat` method takes a stream of `RouteNotes` and returns either another stream of `RouteNotes` or an error.
<!-- $MDX include,file=routeguide/src/client.ml,part=client-route-chat-1 -->
```ocaml
let run_route_chat clock connection =
  (* Generate locations. *)
  let location_count = 5 in
  Printf.printf "Generating %i locations\n" location_count;
  let route_notes =
    location_count
    |> Seq.unfold (function
         | 0 -> None
         | x ->
             Some
               ( RouteNote.make ~location:(random_point ())
                   ~message:(Printf.sprintf "Random Message %i" x)
                   (),
                 x - 1 ))
  in
```

We start by generating a short sequence of locations, similar to how we did for `record_route`.
<!-- $MDX include,file=routeguide/src/client.ml,part=client-route-chat-2 -->
```ocaml
  let rec go writer reader notes =
    match Seq.uncons notes with
    | None ->
        Seq.close_writer writer (* Signal no more notes from the client. *)
    | Some (route_note, xs) -> (
        Seq.write writer route_note;

        (* Yield and sleep, waiting for server reply. *)
        Eio.Time.sleep clock 1.0;
        Eio.Fiber.yield ();

        match Seq.uncons reader with
        | None -> failwith "Expecting response"
        | Some (route_note, reader') ->
            Printf.printf "NOTE = {%s}\n" (RouteNote.show route_note);
            go writer reader' xs)
  in
  let result =
    Client.Typed_rpc.call
      (module RouteGuide.RouteChat)
      ~do_request:(H2_eio.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Typed_rpc.bidirectional_streaming ~f:(fun writer reader ->
             go writer reader route_notes))
      ()
  in
  match result with
  | Ok ((), _ok) -> ()
  | Error e ->
      failwith (Printf.sprintf "HTTP2 error: %s" (H2.Status.to_string e))
```

Then we again use the `Client.Rpc` module to setup a `bidirectional_streaming` function with an interesting type signature `val bidirectional_streaming f:(string Seq.writer -> string Seq.t -> 'a) -> 'a handler`. Somewhat intimidating but hopefully understandable in context. The function `f` represents the writer function for sending notes to the server, with the same semantics as before. Calling it with `Some value` represents sending a value to the stream and `f None` means there is no more data to write. The `string Seq.t` is the stream of `record_note` responses coming back from the server, which we need to decode and print out. We define a recursive function `go` to fold over the list, sending `route_notes`, sleeping to wait for a server response, and printing out that response. When we run out of `route_notes` to send we call `Seq.close_writer f` to tell the server we are done and it can stop listening.
Other combinations of sending and receiving are possible, the reader is encouraged to try them out.

## Try it out!

### Run the server
<!-- $MDX skip -->
```shell
$ dune exec -- routeguide-server ./examples/routeguide/data/route_guide_db.json
```

### Run the client
<!-- $MDX skip -->
```shell
$ dune exec -- routeguide-client
```
