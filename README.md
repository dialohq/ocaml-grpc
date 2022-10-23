# OCaml gRPC

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fjeffa5%2Focaml-grpc%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/jeffa5/ocaml-grpc)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://dialohq.github.io/ocaml-grpc)

Pure OCaml implementation of gRPC over HTTP2.

Supported features:
- RPCs: unary, server streaming, client streaming, bidirectional streaming
- Client (Lwt and Async), server (Lwt only)

To get started, browse the [documentation](https://dialohq.github.io/ocaml-grpc) and the [examples](./examples):
- `examples/greeter-{client,server}-{async,lwt}` implements a basic "Hello world" service using `ocaml-protoc` for Protobuf serialization
- `examples/greeter-{client-tls-async,server-ssl-lwt}` showcases the greeter service over a secure connection
- `examples/etcd` interfaces with an external `etcd` server and shows how to use `ocaml-protoc-plugin` for Protobuf serialization

# Acknowledgments

This MVP version of this library was built by @jeffa5. Andrew gratiously passed the baton to our team to develop it into a full fledged production-ready implementation.
