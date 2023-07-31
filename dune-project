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
 (tags (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.08))
  (uri
   (>= 4.0.0))
  (h2
   (>= 0.9.0))
  ppx_deriving))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `lwt`.")
 (tags (network rpc serialisation))
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
 (tags (network rpc serialisation))
 (depends
  (ocaml
   (>= 4.11))
  (grpc
   (= :version))
  (async (>= v0.16))
  stringext))

(package
 (name grpc-examples)
 (synopsis "Various grpc examples")
 (description "Various grpc examples.")
 (tags (network rpc serialisation))
 (depends
  grpc-lwt
  grpc-async
  grpc-eio
  (ocaml-protoc-plugin (>= 4.5))
  ppx_deriving_yojson
  h2-lwt-unix
  h2-async
  conduit-lwt-unix
  cohttp-lwt-unix
  tls-async
  (lwt_ssl (>= 1.2.0))
  (mdx (and (>= 2.2.1) :with-test))
  (eio_main (>= 0.10))
  stringext))

(package
 (name grpc-bench)
 (synopsis "grpc benchmarking")
 (description "grpc benchmarking.")
 (tags (network rpc serialisation benchmark))
 (depends
  grpc
  (bechamel(>= 0.4.0))
  notty
  (bechamel-notty (>= 0.4.0))
  (bigstringaf (>= 0.9.1))
  (notty (>= 0.2.3))))

(package
 (name grpc-eio)
 (synopsis "An Eio implementation of gRPC")
 (description
  "Functionality for building gRPC services and rpcs with `eio`.")
 (depends
  (grpc
   (= :version))
  h2-eio
  stringext))