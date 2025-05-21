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
 (name grpc))

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
