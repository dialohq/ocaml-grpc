open Bechamel
open Toolkit

let make_buffer capacity =
  Staged.stage @@ fun () -> ignore (Grpc.Buffer.v ~capacity ())

let make_message len =
  Staged.stage @@ fun () ->
  ignore (Grpc.Message.make (String.init len (fun _ -> 'a')))

let extract_message len =
  let msg = Grpc.Message.make (String.init len (fun _ -> 'a')) in
  let bs = Bigstringaf.of_string ~off:0 ~len msg in
  Staged.stage @@ fun () ->
  let buf = Grpc.Buffer.v () in
  Grpc.Buffer.copy_from_bigstringaf ~src_off:0 ~src:bs ~dst:buf ~length:len;
  ignore (Grpc.Message.extract buf)

let test_buffer =
  Test.make_indexed ~name:"buffer" ~fmt:"%s %d"
    ~args:[ 0; 512; 1024; 2048; 4096; 8192 ]
    make_buffer

let test_message =
  Test.make_indexed ~name:"message" ~fmt:"%s %d"
    ~args:[ 0; 32; 64; 128; 256; 512 ]
    make_message

let test_extract =
  Test.make_indexed ~name:"extract" ~fmt:"%s %d"
    ~args:[ 0; 32; 64; 128; 256; 512 ]
    extract_message

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results =
    Benchmark.all cfg instances
      (Test.make_grouped ~name:"make" ~fmt:"%s %s"
         [ test_buffer; test_message; test_extract ])
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
