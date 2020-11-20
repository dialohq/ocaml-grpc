(lang dune 2.7)

(implicit_transitive_deps false)

(generate_opam_files true)

(name grpc)

(license "BSD-3-Clause")

(authors "Andrew Jeffery <dev@jeffas.io>")

(maintainers "Andrew Jeffery <dev@jeffas.io>")

(source
 (github jeffa5/ocaml-grpc))

(package
 (name grpc)
 (synopsis "The core grpc library")
 (depends
  (ocaml
   (>= 4.08))
  (uri
   (>= 4.0.0))
  (ocaml-protoc
   (>= 2.0.0))
  (h2
   (>= 0.7.0))))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of grpc")
 (depends
  (grpc :dev)
  (lwt
   (>= 5.0.0))
  (h2 :dev)))
