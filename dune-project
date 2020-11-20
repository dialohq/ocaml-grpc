(lang dune 2.7)

(generate_opam_files true)

(name grpc)

(package
 (name grpc)
 (synopsis "The core grpc library"))

(package
 (name grpc-lwt)
 (synopsis "An Lwt implementation of grpc")
 (depends
  (grpc :dev)))
