open Printf
open Prelude

let calc m = (m / 3) - 2

let rec calc2 m =
  let next = calc m in
  if next <= 0 then 0 else next + calc2 next

let () =
  Aoc.stdin_seq () |> Seq.map int_of_string |> Seq.map calc
  |> Seq.fold_left ( + ) 0 |> printf "Part 1: %d\n"
  ; Aoc.stdin_seq () |> Seq.map int_of_string |> Seq.map calc2
    |> Seq.fold_left ( + ) 0 |> printf "Part 2: %d\n"
