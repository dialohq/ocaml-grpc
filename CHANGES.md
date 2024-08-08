## Unreleased
- Fix bug in Grpc-lwt-client to fetch status code from response header ([#58](https://github.com/dialohq/ocaml-grpc/pull/58)) ([acerone85](https://github.com/acerone85)) review by ([@quernd](https://github.com/quernd))
- Update Async dependency to v0.17.0 ([#62](https://github.com/dialohq/ocaml-grpc/pull/62) ([@tmcgilchrist](https://github.com/tmcgilchrist))

## 0.2.0 2023-10-23

- Report HTTP2 error status when it's not OK
  ([#22](https://github.com/dialohq/ocaml-grpc/pull/22)) ([@quernd](https://github.com/quernd))
- Fix message extraction behavior for batched messages
  ([#33](https://github.com/dialohq/ocaml-grpc/pull/33)) ([@quernd](https://github.com/quernd))
- Async server implementation
  ([#27](https://github.com/dialohq/ocaml-grpc/pull/27)) ([@tmcgilchrist](https://github.com/tmcgilchrist))
- Eio server and client implementation
  ([#21](https://github.com/dialohq/ocaml-grpc/pull/21)) ([@quernd](https://github.com/quernd))
- Getting started documentation
  ([#25](https://github.com/dialohq/ocaml-grpc/pull/25)) ([@tmcgilchrist](https://github.com/tmcgilchrist))
- Fix a race condition where gRPC status was not reported
  ([#26](https://github.com/dialohq/ocaml-grpc/pull/26)) ([@doctor-pi](https://github.com/doctor-pi))
- OPAM packages for `grpc-examples` and `grpc-bench`
  ([#24](https://github.com/dialohq/ocaml-grpc/pull/24)) ([@tmcgilchrist](https://github.com/tmcgilchrist))

## 0.1.0 2022-10-23

Initial public release: Message transport, Lwt server and client ([@jeffa5](https://github.com/jeffa5)), Async client ([@mbacarella](https://github.com/mbacarella))
Contributions by: [@ansiwen](https://github.com/ansiwen) [@doctor-pi](https://github.com/doctor-pi) [@quernd](https://github.com/quernd) [@wokalski](https://github.com/wokalski)
