(lang dune 3.6)

(generate_opam_files true)
(using mdx 0.2)

(name grpc)

(license "BSD-3-Clause")

(authors
 "Andrew Jeffery <dev@jeffas.io>, Daniel Quernheim <quernd@users.noreply.github.com>, Michael Bacarella <m@bacarella.com>, Sven Anderson <sven@anderson.de>, Wojtek Czekalski <me@wczekalski.com>, dimitris.mostrous <dimitris.mostrous@gmail.com>")

(maintainers "Daniel Quernheim <grpc@quernd.uber.space>")

(source
 (github dialohq/ocaml-grpc))

(documentation "https://dialohq.github.io/ocaml-grpc")

(package
 (name grpc)
 (synopsis "A modular gRPC library")
 (description
  "This library builds some of the signatures and implementations of gRPC functionality. This is used in the more specialised package `grpc-lwt` which has more machinery, however this library can also be used to do some bits yourself.")
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
 (depends
  (ocaml
   (>= 4.11))
  (grpc
   (= :version))
  (h2-async
   (>= 0.9.0))
  stringext))

(package
 (name grpc-examples)
 (synopsis "Various grpc examples")
 (description "Various grpc examples.")
 (depends
  (grpc-lwt (= :version))
  (grpc-async (= :version))
  ocaml-protoc-plugin
  ppx_deriving_yojson
  h2-lwt-unix
  conduit-lwt-unix
  cohttp-lwt-unix
  tls-async
  lwt_ssl
  stringext))

(package
 (name grpc-bench)
 (synopsis "grpc benchmarking")
 (description "grpc benchmarking.")
 (depends
   bechamel
   bechamel-notty
  (grpc (= :version))))