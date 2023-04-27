# Getting Started

This tutorial is meant to be an introduction to OCaml-GRPC and assumes that you have basic [OCaml] experience as well as an understanding of what [protocol buffers] are. If you don't, feel free to read up on the pages linked in this paragraph and come back to this tutorial once you feel you are ready!

[ocaml]: https://ocaml.org/docs/up-and-running
[protocol buffers]: https://developers.google.com/protocol-buffers/docs/overview

## Prerequisites

To run the sample code and walk through this tutorial, the only prerequisite is OCaml itself. To get OCaml insalled follow the [Up and Running]() guide on ocaml.org.

[Up and Running]: https://ocaml.org/docs/up-and-running

## Project Setup

For this tutorial, we will start by creating a new OCaml project with Dune:

<!-- $MDX skip -->
```shell
$ dune init project routeguide
$ cd routeguide
$ opam switch create . 4.14.1 --deps-only --with-test -y
```

`ocaml-grpc` works on OCaml `4.11` and above, the latest `4.14` version will give the best results. This tutorial uses LWT, a concurrent programming library for OCaml, other options exist like Async or EIO example code for both exists in [examples].

## Defining the HelloWorld service

The first step is to define the gRPC *service*, the method *request* and *response* types using [protocol buffers]. We will keep our `.proto` files in a directory in our project's root. There is no requirement where the `.proto` definitions live, [Dune] will build them regardless.

<!-- $MDX skip -->
```shell
$ mkdir proto && touch proto/helloworld.proto
```

Then the RPC methods get defined inside the service defintion, specifying the request and response types. gRPC lets you define four kinds of service methods, all of which are supported in OCaml. For this tutorial we will only use a simple RPC, if you would like to see an OCaml example which uses all four kinds please read the [routeguide tutorial].

[routeguide tutorial]: https://github.com/dialohq/ocaml-grpc/blob/master/examples/routeguide-tutorial.md
[Dune]: https://dune.readthedocs.io

First we define our package name, which is the package OCaml looks for when including your protos in the client and server applications. Lets give this one a name of `helloworld`.

<!-- $MDX skip -->
```protocol-buffer
syntax = "proto3";
package helloworld;
```

Next we need to define our service. This service will contain the actual RPC calls we will be using in our application. An RPC contains an Identifier, a Request type, and returns a Response type. Here is our Greeter service which provides the *SayHello* RPC method.

<!-- $MDX skip -->
```protocol-buffer
service Greeter {
    // Our SayHello rpc accepts HelloRequests and returns HelloReplies
    rpc SayHello (HelloRequest) returns (HelloReply);
}
```

Next we have to define the types used in our `SayHello` RPC method. RPC types are defined as messages which contain typed fields. The messages for our HelloWorld application look like this.

<!-- $MDX skip -->
```protocol-buffer
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

<!-- $MDX skip -->
```protocol-buffer
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

<!-- $MDX skip -->
```shell
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
  (ocaml-protoc-plugin (>= 4.4.0)))
```

We include `ocaml-protoc-plugin` as a useful way to incorporate the generation of our client and server gRPC code into the build process of our application. We will setup this build process now.

## Generating Server and Client code

In the proto directory of your project create a `dune` file and add the following code:

<!-- $MDX include,file=greeter/dune -->
```ocaml
(library
 (name greeter)
 (public_name grpc-examples)
 (libraries ocaml-protoc-plugin))

(rule
 (targets greeter.ml)
 (deps
  (:proto greeter.proto))
 (action
  (run
    protoc
    -I
    .
    "--ocaml_out=annot=[@@deriving show { with_path = false }]:."
    %{proto})))
```

Make sure the dependencies are installed

<!-- $MDX skip -->
```shell
$ opam install dune grpc-lwt.0.1.0 ocaml-protoc-plugin.4.4.0 -y
```

This build configuration file tells dune to compile your protobufs when you build your OCaml project. It uses the `ocaml-protoc` cli tool and bundles up the OCaml code as a library called `helloworld`, which you depend on when writing the gRPC server and client.

## Writing our Server

Now that the build process is written and our dependencies are all setup, we can begin writing the fun stuff! We need to import the things we will be using in our server, including the protobuf. Start by making a file called server.ml in your `/lib` directory and writing the following code:

<!-- $MDX include,file=greeter-lwt/server.ml,part=server-imports -->
```ocaml
open Grpc_lwt
```

Next up, we implement the Greeter service we previously defined in our `.proto` file. He is what that might look like:

<!-- $MDX include,file=greeter-lwt/server.ml,part=server-hello -->
```ocaml
let say_hello buffer =
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let (decode, encode) = Service.make_service_functions Greeter.sayHello in
  let request =
    Reader.create buffer
    |> decode
    |> function
      | Ok v -> v
      | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  let message =
    if request = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" request
  in
  let reply = Greeter.SayHello.Response.make ~message () in
  Lwt.return (Grpc.Status.(v OK), Some (encode reply |> Writer.contents ))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"SayHello" ~rpc:(Unary say_hello) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)
```

Finally, let's define the LWT runtime that our server will actually run on.

<!-- $MDX include,file=greeter-lwt/server.ml,part=server-main -->
```ocaml
let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| dune exec -- examples/greeter-client-lwt/greeter_client_lwt.exe <your_name> |});

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
```

You should now be able to run your HelloWorld gRPC server using the command `dune exec -- service`. This uses the executable we defined earlier in our `lib/dune` to specifically run the server. Consult the full source code in [greeter-lwt/service.ml](greeter-lwt/service.ml) if you encounter errors.

If you have a gRPC GUI client such as [Bloom RPC] you should be able to send requests to the server and get back greetings!

Or if you use [grpcurl] then you can simply try send requests like this:
<!-- $MDX skip -->
```shell
$ grpcurl -plaintext -import-path ./proto -proto helloworld.proto -d '{"name": "OCaml"}' '[localhost]:50051' helloworld.Greeter/SayHello
```
And receiving responses like this:
<!-- $MDX skip -->
```shell
{
  "message": "Hello Tonic!"
}
```

[bloom rpc]: https://github.com/uw-labs/bloomrpc
[grpcurl]: https://github.com/fullstorydev/grpcurl

## Writing our Client

So now we have a running gRPC server, and that's great but how can our application communicate with it? This is where our client would come in. ocaml-grpc supports both client and server implementations. Similar to the server, we will start by creating a file client.ml in our `/lib` directory and importing everything we will need:

<!-- $MDX include,file=greeter-lwt/client.ml,part=client-imports -->
```ocaml
open Grpc_lwt
open Lwt.Syntax
```

The client is much simpler than the server as we don't need to implement any service methods, just make requests. Here is an Lwt runtime which will make our request and print the response to your terminal:

<!-- $MDX include,file=greeter-lwt/client.ml,part=client-hello -->
```ocaml
let call_server address port req =
  (* Setup Http/2 connection *)
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
  let open Ocaml_protoc_plugin in
  let open Greeter.Mypackage in
  let encode, decode = Service.make_client_functions Greeter.sayHello in
  let enc = encode req |> Writer.contents in

  Client.call ~service:"mypackage.Greeter" ~rpc:"SayHello"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder -> (
               Reader.create decoder |> decode |> function
               | Ok v -> v
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> Greeter.SayHello.Response.make ()))
    ()
```

Then we run the client as:

<!-- $MDX include,file=greeter-lwt/client.ml,part=client-main -->
```ocaml
let () =
  let open Lwt.Syntax in
  let port = 8080 in
  let address = "localhost" in
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Greeter.Mypackage.HelloRequest.make ~name () in
  Lwt_main.run
    (let+ res = call_server address port req in
     match res with
     | Ok (res, _) -> print_endline res
     | Error _ -> print_endline "an error occurred")
```

That's it! Our complete client source is available as [greeter-lwt/client.ml](greeter-lwt/client.ml).

## Putting it all together

At this point we have written our protobuf file, a couple of dune files to compile our protobuf and OCaml code, a server which implements our SayHello service, and a client which makes requests to our server. You should have a project that looks like this:

<!-- $MDX skip -->
```shell
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
