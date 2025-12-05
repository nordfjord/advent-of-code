open Base
open Stdio

let lines = In_channel.input_lines stdin
let to_int c = Char.to_int c - Char.to_int '0'

let solve k str =
  let n = String.length str in
  let find_max_in_range start end_pos =
    let rec aux i max_idx =
      if i > end_pos
      then max_idx
      else if Char.(str.[i] > str.[max_idx])
      then aux (i + 1) i
      else aux (i + 1) max_idx
    in
    aux (start + 1) start
  in
  let rec max_joltage i next_start acc =
    if i >= k
    then acc
    else (
      let remaining = k - i - 1 in
      let max_end = n - remaining - 1 in
      let max_idx = find_max_in_range next_start max_end in
      max_joltage (i + 1) (max_idx + 1) ((acc * 10) + to_int str.[max_idx]))
  in
  max_joltage 0 0 0

let () =
  List.sum (module Int) lines ~f:(solve 2) |> printf "Part 1: %d\n";
  List.sum (module Int) lines ~f:(solve 12) |> printf "Part 2: %d\n"
