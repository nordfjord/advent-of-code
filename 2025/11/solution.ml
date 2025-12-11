open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()
let lines = In_channel.input_lines stdin

let parse_line line =
  match Str.split (Str.regexp ": ") line with
  | [ key; values ] ->
    let value_list = String.split ~on:' ' values in
    (key, value_list)
  | _ -> failwith "Invalid line format"

let data = List.map lines ~f:parse_line

let graph =
  let tbl = Hashtbl.create (module String) in
  List.iter data ~f:(fun (key, values) -> Hashtbl.add_exn tbl ~key ~data:values);
  tbl

let count_paths start target =
  let memo = Hashtbl.create (module String) in
  let rec aux u v =
    if String.equal u v
    then 1
    else (
      match Hashtbl.find memo u with
      | Some count -> count
      | None ->
        let neighbors = Hashtbl.find_multi graph u in
        let total =
          List.fold neighbors ~init:0 ~f:(fun acc neighbor -> acc + aux neighbor v)
        in
        Hashtbl.add_exn memo ~key:u ~data:total;
        total)
  in
  aux start target

let part1 () = count_paths "you" "out"

let part2 () =
  let svr_to_dac = count_paths "svr" "dac" in
  let dac_to_fft = count_paths "dac" "fft" in
  let fft_to_out = count_paths "fft" "out" in
  let path1 = svr_to_dac * dac_to_fft * fft_to_out in
  let svr_to_fft = count_paths "svr" "fft" in
  let fft_to_dac = count_paths "fft" "dac" in
  let dac_to_out = count_paths "dac" "out" in
  let path2 = svr_to_fft * fft_to_dac * dac_to_out in
  path1 + path2

(* count_all_paths_containing "svr" "out" [ "dac"; "fft" ] *)

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
