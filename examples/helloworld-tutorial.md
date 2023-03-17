# Getting Started

This tutorial is meant to be an introduction to OCaml-GRPC and assumes that you have basic [OCaml] experience as well as an understanding of what [protocol buffers] are. If you don't, feel free to read up on the pages linked in this paragraph and come back to this tutorial once you feel you are ready!

[ocaml]: https://ocaml.org/docs/up-and-running
[protocol buffers]: https://developers.google.com/protocol-buffers/docs/overview

## Prerequisites

To run the sample code and walk through this tutorial, the only prerequisite is OCaml itself. To get OCaml insalled follow the [Up and Running]() guide on ocaml.org.

[Up and Running]: https://ocaml.org/docs/up-and-running

## Project Setup

For this tutorial, we will start by creating a new OCaml project with Dune:

``` shell
$ dune init project routeguide
$ cd routeguide
$ opam switch create . 4.14.1 --deps-only --with-test -y
```

`ocaml-grpc` works on OCaml `4.11` and above, the latest `4.14` version will give the best results. This tutorial uses LWT, a concurrent programming library for OCaml, other options exist like Async or EIO example code for both exists in [examples].

## Defining the HelloWorld service

The first step is to define the gRPC *service*, the method *request* and *response* types using [protocol buffers]. We will keep our `.proto` files in a directory in our project's root. There is no requirement where the `.proto` definitions live, [Dune] will build them regardless.

``` shell
$ mkdir proto && touch proto/helloworld.proto
```

Then the RPC methods get defined inside the service defintion, specifying the request and response types. gRPC lets you define four kinds of service methods, all of which are supported in OCaml. For this tutorial we will only use a simple RPC, if you would like to see an OCaml example which uses all four kinds please read the [routeguide tutorial].

[routeguide tutorial]: https://github.com/dialohq/ocaml-grpc/blob/master/examples/routeguide-tutorial.md
[Dune]: https://dune.readthedocs.io

First we define our package name, which is the package OCaml looks for when including your protos in the client and server applications. Lets give this one a name of `helloworld`.

``` protocol-buffer
syntax = "proto3";
package helloworld;
```

Next we need to define our service. This service will contain the actual RPC calls we will be using in our application. An RPC contains an Identifier, a Request type, and returns a Response type. Here is our Greeter service which provides the *SayHello* RPC method.

``` protocol-buffer
service Greeter {
    // Our SayHello rpc accepts HelloRequests and returns HelloReplies
    rpc SayHello (HelloRequest) returns (HelloReply);
}
```

Next we have to define the types used in our `SayHello` RPC method. RPC types are defined as messages which contain typed fields. The messages for our HelloWorld application look like this.

``` protocol-buffer
message HelloRequest {
    // Request message contains the name to be greeted
    string name = 1;
}

message HelloReply {
    // Reply contains the greeting message
    string message = 1;
}
```

Super! Now our `.proto` file should be complete and ready for use in our application. Here is what it should look like completed:

``` protocol-buffer
syntax = "proto3";
package helloworld;

service Greeter {
    rpc SayHello (HelloRequest) returns (HelloReply);
}

message HelloRequest {
   string name = 1;
}

message HelloReply {
    string message = 1;
}
```

## Application Setup 
Now that we have defined the protobuf for our application we can start writing our OCaml. First we need to add our required dependencies to the `dune-project`.

``` shell
(lang dune 3.7)

(name helloworld_grpc)
(generate_opam_files true)

(package
 (name helloworld_grpc)
 (synopsis "Helloworld in gRPC")
 (description "Helloworld in gRPC.")
 (depends
  (grpc-lwt (>= 0.1.0))
  (h2-lwt-unix (>= 0.9.0))
  (ocaml-protoc (>= 2.4)))
```

We include `ocaml-protoc` as a useful way to incorporate the generation of our client and server gRPC code into the build process of our application. We will setup this build process now.

## Generating Server and Client code

In the proto directory of your project create a `dune` file and add the following code:

``` shell
(library
 (name helloworld)
 (libraries ocaml-protoc))

(rule
 (targets
   helloworld_pb.ml
   helloworld_pb.mli
   helloworld_pp.ml
   helloworld_pp.mli
   helloworld_types.ml
   helloworld_types.mli)
 (action
  (run ocaml-protoc -binary -pp -ml_out ./ helloworld.proto))
 (deps helloworld.proto))
```

Make sure the dependencies are installed
``` shell
$ opam install dune grpc-lwt.0.1.0 ocaml-protoc.2.4 -y
```

This build configuration file tells dune to compile your protobufs when you build your OCaml project. It uses the `ocaml-protoc` cli tool and bundles up the OCaml code as a library called `helloworld`, which you depend on when writing the gRPC server and client.

## Writing our Server

Now that the build process is written and our dependencies are all setup, we can begin writing the fun stuff! We need to import the things we will be using in our server, including the protobuf. Start by making a file called server.ml in your `/lib` directory and writing the following code:

``` ocaml
open Grpc_lwt
```

Next up, we implement the Greeter service we previously defined in our `.proto` file. He is what that might look like:

``` ocaml
let say_hello buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  let req = Helloworld.Helloworld_pb.decode_hello_request decoder in
  let message =
    if req.name = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" req.name
  in
  let reply = Helloworld.Helloworld_types.default_hello_reply ~message () in
  let encoder = Pbrt.Encoder.create () in
  Helloworld.Helloworld_pb.encode_hello_reply reply encoder;
  Lwt.return (Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"helloworld.Greeter" ~service:greeter_service)
```

Finally, let's define the LWT runtime that our server will actually run on.

``` ocaml
let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let* _server = Lwt_io.establish_server_with_client_socket listen_address server in
      let* () = Lwt_io.printf "Listening on port %i for grpc requests\n" port in
      Lwt_io.(flush stdout));

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
```

All together your server should look something like this once you are done:

``` ocaml
open Grpc_lwt

let say_hello buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  let req = Helloworld.Helloworld_pb.decode_hello_request decoder in
  let message =
    if req.name = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" req.name
  in
  let reply = Helloworld.Helloworld_types.default_hello_reply ~message () in
  let encoder = Pbrt.Encoder.create () in
  Helloworld.Helloworld_pb.encode_hello_reply reply encoder;
  Lwt.return (Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"helloworld.Greeter" ~service:greeter_service)

let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let listen_address = Unix.(ADDR_INET (inet_addr_any, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let* _server = Lwt_io.establish_server_with_client_socket listen_address server in
      let* () = Lwt_io.printf "Listening on port %i for grpc requests\n" port in
      Lwt_io.(flush stdout));

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
```
You should now be able to run your HelloWorld gRPC server using the command `dune exec -- service`. This uses the executable we defined earlier in our `lib/dune` to run specifically the server.

If you have a gRPC GUI client such as [Bloom RPC] you should be able to send requests to the server and get back greetings!

Or if you use [grpcurl] then you can simply try send requests like this:
```
$ grpcurl -plaintext -import-path ./proto -proto helloworld.proto -d '{"name": "OCaml"}' '[localhost]:50051' helloworld.Greeter/SayHello
```
And receiving responses like this:
```
{
  "message": "Hello Tonic!"
}
```

[bloom rpc]: https://github.com/uw-labs/bloomrpc
[grpcurl]: https://github.com/fullstorydev/grpcurl

## Writing our Client

So now we have a running gRPC server, and that's great but how can our application communicate with it? This is where our client would come in. ocaml-grpc supports both client and server implementations. Similar to the server, we will start by creating a file client.ml in our `/lib` directory and importing everything we will need:

``` ocaml
open Grpc_lwt
open Lwt.Syntax
```

The client is much simpler than the server as we don't need to implement any service methods, just make requests. Here is an Lwt runtime which will make our request and print the response to your terminal:

``` ocaml
let call_server address port req =
  (* Setup HTTP2 socket with LWT *)
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    H2_lwt_unix.Client.create_connection ~error_handler socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Helloworld.Helloworld_pb.encode_hello_request req enc;

  Client.call ~service:"helloworld.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary (Pbrt.Encoder.to_string enc) ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder ->
               let decoder = Pbrt.Decoder.of_string decoder in
               Helloworld.Helloworld_pb.decode_hello_reply decoder
           | None -> Helloworld.Helloworld_types.default_hello_reply ()))
    ()

let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Helloworld.Helloworld_types.default_hello_request ~name () in
  Lwt_main.run
    (let+ res = call_server address port req in
     match res with
     | Ok (res, _) -> print_endline res.message
     | Error _ -> print_endline "an error occurred")
```

That's it! Our complete client file should look something like below, if it doesn't please go back and make sure you followed along correctly:

``` ocaml
open Grpc_lwt
open Lwt.Syntax

let call_server address port req =
  (* Setup HTTP2 socket with LWT *)
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    H2_lwt_unix.Client.create_connection ~error_handler socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Helloworld.Helloworld_pb.encode_hello_request req enc;

  Client.call ~service:"helloworld.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary (Pbrt.Encoder.to_string enc) ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder ->
               let decoder = Pbrt.Decoder.of_string decoder in
               Helloworld.Helloworld_pb.decode_hello_reply decoder
           | None -> Helloworld.Helloworld_types.default_hello_reply ()))
    ()

let () =
  let open Lwt.Syntax in
  let port = 50051 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Helloworld.Helloworld_types.default_hello_request ~name () in
  Lwt_main.run
    (let+ res = call_server address port req in
     match res with
     | Ok (res, _) -> print_endline res.message
     | Error _ -> print_endline "an error occurred")
```

## Putting it all together

At this point we have written our protobuf file, a couple of dune files to compile our protobuf and OCaml code, a server which implements our SayHello service, and a client which makes requests to our server. You should have a project that looks like this:

``` shell
$ tree -L 2
.
├── dune-project
├── helloworld_grpc.opam
├── lib
│   ├── client.ml
│   ├── dune
│   └── service.ml
└── proto
    ├── dune
    └── helloworld.proto
```

To run the server use `dune exec -- server` and for the client use `dune exec -- client OCaml` in another terminal window.

You should see the request logged out by the server in its terminal window, as well as the response logged out by the client in its window.

Congrats on making it through this introductory tutorial! We hope that this walkthrough tutorial has helped you understand the basics of OCaml gRPC, and how to get started writing high-performance, interoperable, and flexible gRPC servers in Ocaml. For a more in-depth tutorial which showcases an advanced gRPC server in OCaml, please see the [routeguide tutorial].

[routeguide tutorial]: https://github.com/dialohq/ocaml-grpc/blob/master/examples/routeguide-tutorial.md
[examples]: https://github.com/dialohq/ocaml-grpc/blob/master/examples