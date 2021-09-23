(lang dune 2.7)

(implicit_transitive_deps false)

(generate_opam_files true)

(name grpc)

(license "BSD-3-Clause")

(authors "Andrew Jeffery <dev@jeffas.io>")

(maintainers "Andrew Jeffery <dev@jeffas.io>")

(source
 (github jeffa5/ocaml-grpc))

(documentation "https://jeffas.io/ocaml-grpc")

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
   (>= 0.7.0))
  ppx_deriving))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of gRPC")
 (description "Functionality for building gRPC services and rpcs with `lwt`.")
 (depends
  grpc
  (lwt
   (>= 5.0.0))
  (h2
   (>= 0.8.0))
  stringext))
