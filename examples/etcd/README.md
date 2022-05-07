# Etcd proxy

This example showcases communication with [etcd](https://etcd.io/), inspired by [the `ocaml-grpc-envoy` example](https://github.com/blandinw/ocaml-grpc-envoy). It implements a proxy for the two methods `etcdserverpb.KV/Put` and `etcdserverpb.KV/Range` (a small subset of the full `etcd` protocol) and showcases a persistent gRPC connection between requests.

## How to run

1. Run `etcd`
2. Run this proxy: `dune exec examples/etcd/etcd_proxy.exe -- ETCD_HOST ETCD_PORT PORT`, e.g. `dune exec examples/etcd/etcd_proxy.exe -- 127.0.0.1 2379 8080`
3. Make requests using `GET` and `POST`. The key will be the path of the request URI. A `GET` request will call `Range` to read the value for the given key, a `POST` request will call `Put` to store the value (the request body), e.g.

```
curl -XPOST localhost:8080/key -d'value'
curl localhost:8080/key
```

4. The proxy will return both the response and the original gRPC status, both in the request body.
