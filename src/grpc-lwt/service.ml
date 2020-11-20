module RpcMap = Map.Make (String)

type 'a t = { name : string; rpcs : Rpc.t RpcMap.t; state : 'a }

let v ~name ~state = { name; rpcs = RpcMap.empty; state }

let add_rpc name rpc t = { t with rpcs = RpcMap.add name rpc t.rpcs }

let name t = t.name

let handle_rpc _reqd = Ok ()
