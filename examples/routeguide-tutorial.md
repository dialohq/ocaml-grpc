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

``` shell
$ git clone https://github.com/dialohq/ocaml-grpc 
...
$ cd ocaml-grpc
...
```

Run the server 

``` shell
$ dune exec -- routeguide-server
```

In a separate shell, run the client

``` shell
$ dune exec -- routeguide-client
```

You should see some logging output in both terminal windows. On the shell where you ran the client binary, you should see the output of the bi-directional streaming rpc, printing 1 line per second

``` shell
NOTE = RouteNote { location: Some(Point { latitude: 409146139, longitude: -746188906 }), message: "at 1.000319208s" }
```

If you scroll up you should see the output of the other three request types; simple rpc, server-side streaming, and client-side streaming.

## Project setup

For this tutorial, we will start by creating a new OCaml project with Dune:

``` shell
$ dune init project routeguide
$ cd routeguide
$ opam switch create . 4.14.1 --deps-only --with-test -y
```

`ocaml-grpc` works on OCaml `4.11` and above, the latest `4.14` version will give the best results. This tutorial uses LWT, a concurrent programming library for OCaml, other options exist like Async or EIO example code for both exists in [examples].

Our first step is to define the gRPC service and the method *request* and *response* types using [protocol buffers](). We will keep our `.proto` files in a directory in our project's root. There is no requirement where the `.proto` definitions live, [Dune]() will build them regardless.

``` shell
$ mkdir proto && touch proto/route_guide.proto
```

You can see the complete `.proto` file in [examples/routeguide/proto/route_guide.proto](). 

To define a service, first we define a *service` in your *.proto* file:

``` protocol-buffer
service RouteGuide {
   ...
}
```

Then you define `rpc` methods inside this service definition, specifying their *request* and *response* types. gRPC lets you define four kinds of service methods, all of which will be used in the `RouteGuide` service:

 * A *simple RPC* where the client sends a request to the server and waits for a response to come back. Just like a normal function call.
``` protocol-buffer
  // Obtains the feature at a given position.
  rpc GetFeature(Point) returns (Feature) {}
```
