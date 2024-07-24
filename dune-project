(lang dune 3.7)

(generate_opam_files true)

(using mdx 0.3)

(name grpc)

(license "BSD-3-Clause")

(authors
 "Andrew Jeffery <dev@jeffas.io>"
 "Daniel Quernheim <quernd@users.noreply.github.com>"
 "Michael Bacarella <m@bacarella.com>"
 "Sven Anderson <sven@anderson.de>"
 "Tim McGilchrist <timmcgil@gmail.com>"
 "Wojtek Czekalski <me@wczekalski.com>"
 "dimitris.mostrous <dimitris.mostrous@gmail.com>")

(maintainers "Daniel Quernheim <grpc@quernd.uber.space>")

(cram enable)

(source
 (github dialohq/ocaml-grpc))

(documentation "https://dialohq.github.io/ocaml-grpc")

(package
 (name grpc)
 (synopsis "A modular gRPC library")
 (description
  "This library contains the implementation of (de)serialization of gRPC messages and statuses.")
 (tags
  (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.08))
  (bigstringaf
   (>= 0.9.1))
  ppx_deriving
  (uri
   (>= 4.0.0))))

(package
 (name grpc-server)
 (synopsis "Reusable logic for server side gRPC")
 (description
  "All modules are networking-layer and concurrency-layer agnostic.")
 (tags
  (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.08))
  (grpc (= :version))))

(package
 (name grpc-client)
 (synopsis "Reusable logic for client side gRPC")
 (description
  "All modules are networking-layer and concurrency-layer agnostic.")
 (tags
  (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.08))
  (grpc (= :version))))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `lwt`.")
 (tags
  (network rpc serialisation))
 (depends
  (grpc-server
   (= :version))
  (h2
   (>= 0.9.0))
  (lwt
   (>= 5.3.0))
  stringext))

(package
 (name grpc-async)
 (synopsis "An Async implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `async`.")
 (tags
  (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.11))
  (async
   (>= v0.16))
  (grpc-server
   (= :version))
  (h2
   (>= 0.9.0))
  (ppx_jane
   (>= v0.16.0))
  stringext))

(package
 (name grpc-server-eio)
 (deprecated_package_names grpc-eio)
 (synopsis "An Eio implementation of gRPC server")
 (description
  "Functionality for building gRPC services and rpcs with `eio`.")
 (depends
  (eio
   (>= 0.12))
  (grpc-server
   (= :version))
  stringext))

(package
 (name grpc-client-eio)
 (synopsis "An Eio implementation of gRPC client")
 (description
  "Functionality for building gRPC services and rpcs with `eio`.")
 (depends
  (eio
   (>= 0.12))
  (grpc-client
   (= :version))))

(package
 (name grpc-eio-io-client-h2-ocaml-protoc)
 (synopsis "An h2 implementation of gRPC networking layer for eio based clients.")
 (depends
  (grpc-client-eio
   (= :version))
  (h2
   (>= 0.9.0))
  pbrt
  pbrt_services
  eio
  h2-eio
  grpc-eio-core))

(package
 (name grpc-eio-io-server-h2-ocaml-protoc)
 (synopsis "An h2 implementation of gRPC networking layer for eio based servers.")
 (depends
  (grpc-server-eio
   (= :version))
  (h2
   (>= 0.9.0))
  stringext
  pbrt
  pbrt_services
  eio
  h2-eio
  grpc-eio-core))


(package
 (name arpaca)
 (synopsis "An Eio implementation of gRPC client")
 (description
  "Functionality for building gRPC services and rpcs with `eio`.")
 (depends
  (grpc
   (= :version))
  (grpc-client-eio
    (= :version))
  (grpc-client
   (= :version))))

(package
 (name grpc-examples)
 (synopsis "Various gRPC examples")
 (description "Various gRPC examples.")
 (tags
  (network rpc serialisation))
 (depends
  (async
   (>= v0.16.0))
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  conduit-lwt-unix
  (core
   (>= v0.16.2))
  (core_unix
   (>= v0.16.0))
  (eio_main
   (>= 0.12))
  grpc-async
  grpc-eio
  grpc-lwt
  (h2
   (>= 0.9.0))
  h2-async
  h2-eio
  h2-lwt-unix
  (lwt
   (>= 5.3.0))
  (lwt_ssl
   (>= 1.2.0))
  (mdx
   (and
    (>= 2.2.1)
    :with-test))
  (ocaml-protoc-plugin
   (>= 4.5))
  ppx_deriving
  ppx_deriving_yojson
  (ppx_jane
   (>= v0.16.0))
  stringext
  tls-async
  (uri
   (>= 4.0.0))))

(package
 (name grpc-bench)
 (synopsis "Benchmarking package for gRPC")
 (description "Benchmarking package for gRPC.")
 (tags
  (network rpc serialisation benchmark))
 (depends
  (bechamel
   (>= 0.4.0))
  (bechamel-notty
   (>= 0.4.0))
  (bigstringaf
   (>= 0.9.1))
  grpc
  (notty
   (>= 0.2.3))))

(package
 (name grpc-eio-core)
 (synopsis "Benchmarking package for gRPC")
 (description "Benchmarking package for gRPC.")
 (tags
  (network rpc serialisation benchmark))
 (depends
  eio))
