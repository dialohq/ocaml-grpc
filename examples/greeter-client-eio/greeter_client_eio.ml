let main env =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let network = Eio.Stdenv.net env in
  let run sw =
    let open Ocaml_protoc_plugin in
    let open Greeter.Mypackage in
    let encode, decode = Service.make_client_functions Greeter.sayHello in

    let net =
      Grpc_eio_net_client_h2.create_client ~sw ~net:network
        "http://localhost:8080"
    in

    let result =
      Grpc_client_eio.Client.unary ~sw ~net ~service:"mypackage.Greeter"
        ~method_name:"SayHello"
        ~encode:(fun x -> x |> encode |> Writer.contents)
        ~decode:(fun x -> Reader.create x |> decode)
        ~headers:(Grpc_client.make_request_headers `Proto)
        (HelloRequest.make ~name ())
    in
    match result with
    | Ok message -> Eio.traceln "%s" message
    | Error (`Rpc (response, status)) ->
        Eio.traceln "Error: %a, %a" H2.Status.pp_hum response.status
          Grpc.Status.pp status
    | Error (`Connection _err) -> Eio.traceln "Connection error"
    | Error (`Decoding err) ->
        Eio.traceln "Decoding error: %a" Ocaml_protoc_plugin.Result.pp_error err
  in
  Eio.Switch.run run

let () = Eio_main.run main
