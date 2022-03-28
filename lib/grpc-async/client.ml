open! Core
open! Async

type response_handler = H2.Client_connection.response_handler

type do_request =
  ?trailers_handler:(H2.Headers.t -> unit) ->
  H2.Request.t ->
  response_handler:response_handler ->
  [ `write ] H2.Body.t

let make_request ~scheme ~service ~rpc ~headers =
  let request =
    H2.Request.create ~scheme `POST ("/" ^ service ^ "/" ^ rpc) ~headers
  in
  request

let default_headers =
  H2.Headers.of_list
    [ ("te", "trailers"); ("content-type", "application/grpc+proto") ]

let call ~service ~rpc ?(scheme = "https") ~handler ~do_request
    ?(headers = default_headers) () =
  let request = make_request ~service ~rpc ~scheme ~headers in
  let read_body_ivar = Ivar.create () in
  let handler_res_ivar = Ivar.create () in
  let out_ivar = Ivar.create () in
  let trailers_status_ivar = Ivar.create () in
  let trailers_handler headers =
    let code =
      match H2.Headers.get headers "grpc-status" with
      | None -> None
      | Some s -> (
          match int_of_string_opt s with
          | None -> None
          | Some i -> Grpc.Status.code_of_int i)
    in
    match code with
    | None -> ()
    | Some code -> 
        match Ivar.is_empty trailers_status_ivar with 
        | true -> 
          let message = H2.Headers.get headers "grpc-message" in
          let status = Grpc.Status.v ?message code in
          Ivar.fill trailers_status_ivar status
        | _ -> (* This should never happen, but just in case. *) ()
  in
  let response_handler (response : H2.Response.t) (body : [ `read ] H2.Body.t) =
    Ivar.fill read_body_ivar body;
    don't_wait_for
      (match response.status with
      | `OK ->
          let%bind handler_res = Ivar.read handler_res_ivar in
          Ivar.fill out_ivar (Ok handler_res);
          return ()
      | _ ->
          Ivar.fill out_ivar (Error (Grpc.Status.v Grpc.Status.Unknown));
          return ());
    don't_wait_for (return (trailers_handler response.headers))
  in
  let write_body : [ `write ] H2.Body.t =
    do_request ?trailers_handler:(Some trailers_handler) request
      ~response_handler
  in
  don't_wait_for
    (let%bind handler_res = handler write_body (Ivar.read read_body_ivar) in
     Ivar.fill handler_res_ivar handler_res;
     return ());
  let%bind out = Ivar.read out_ivar in
  let%bind trailers_status =
    (* In case no grpc-status appears in headers or trailers. *)
    if Ivar.is_full trailers_status_ivar then Ivar.read trailers_status_ivar
    else return (
      Grpc.Status.v ~message:"Server did not return grpc-status" Grpc.Status.Unknown) 
  in
  match out with
  | Error _ as e -> return e
  | Ok out -> return (Ok (out, trailers_status))

module Rpc = struct
  type 'a handler =
    [ `write ] H2.Body.t -> [ `read ] H2.Body.t Deferred.t -> 'a Deferred.t

  let bidirectional_streaming ~handler write_body read_body =
    let decoder_r, decoder_w = Async.Pipe.create () in
    don't_wait_for
      (let%map read_body = read_body in
       Connection.grpc_recv_streaming read_body decoder_w);
    let encoder_r, encoder_w = Async.Pipe.create () in
    don't_wait_for (Connection.grpc_send_streaming_client write_body encoder_r);
    let%bind out = handler encoder_w decoder_r in
    if not (Pipe.is_closed encoder_w) then Pipe.close encoder_w;
    if not (Pipe.is_closed decoder_w) then Pipe.close decoder_w;
    return out

  let client_streaming ~handler write_body read_body =
    bidirectional_streaming
      ~handler:(fun encoder_w _decoder_r -> handler encoder_w)
      write_body read_body

  let server_streaming ~handler ~encoded_request write_body read_body =
    bidirectional_streaming
      ~handler:(fun encoder_w decoder_r ->
        Async.Pipe.write_without_pushback encoder_w encoded_request;
        Async.Pipe.close encoder_w;
        handler decoder_r)
      write_body read_body

  let unary ~handler ~encoded_request write_body read_body =
    bidirectional_streaming
      ~handler:(fun encoder_w decoder_r ->
        Async.Pipe.write_without_pushback encoder_w encoded_request;
        Async.Pipe.close encoder_w;
        match%bind Async.Pipe.read decoder_r with
        | `Eof -> handler None
        | `Ok a -> handler (Some a))
      write_body read_body
end
