open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

let parse_line line =
  match Str.split (Str.regexp ": ") line with
  | [ key; values ] ->
    let value_list = String.split ~on:' ' values in
    (key, value_list)
  | _ -> failwith "Invalid line format"

let graph =
  In_channel.input_lines stdin
  |> List.map ~f:parse_line
  |> Hashtbl.of_alist_exn (module String)

let count_paths start target =
  let dfs =
    Prelude.Func.memo_rec (fun aux current ->
      if String.equal current target
      then 1
      else Hashtbl.find_multi graph current |> List.sum (module Int) ~f:aux)
  in
  dfs start

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

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
