open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

let lines = In_channel.input_lines stdin

let part1 () = 0
let part2 () = 0

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms
" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
