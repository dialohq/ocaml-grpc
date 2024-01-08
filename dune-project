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

(source
 (github dialohq/ocaml-grpc))

(documentation "https://dialohq.github.io/ocaml-grpc")

(package
 (name grpc)
 (synopsis "A modular gRPC library")
 (description
  "This library builds some of the signatures and implementations of gRPC functionality. This is used in the more specialised package `grpc-lwt` which has more machinery, however this library can also be used to do some bits yourself.")
 (tags
  (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.08))
  (h2
   (>= 0.9.0))
  ppx_deriving
  (uri
   (>= 4.0.0))))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `lwt`.")
 (tags
  (network rpc serialisation))
 (depends
  (grpc
   (= :version))
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
  (grpc
   (= :version))
  stringext))

(package
 (name grpc-eio)
 (synopsis "An Eio implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `eio`.")
 (depends
  (eio
   (>= 0.12))
  (grpc
   (= :version))
  stringext))

(package
 (name grpc-examples)
 (synopsis "Various gRPC examples")
 (description "Various gRPC examples.")
 (tags
  (network rpc serialisation))
 (depends
  cohttp-lwt-unix
  conduit-lwt-unix
  (eio_main
   (>= 0.12))
  grpc-async
  grpc-eio
  grpc-lwt
  h2-async
  h2-eio
  h2-lwt-unix
  (lwt_ssl
   (>= 1.2.0))
  (mdx
   (and
    (>= 2.2.1)
    :with-test))
  (ocaml-protoc-plugin
   (>= 4.5))
  ppx_deriving_yojson
  stringext
  tls-async))

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
