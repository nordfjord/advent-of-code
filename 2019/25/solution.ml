open Base
open Stdio

let sw = Prelude.Stopwatch.start ()

let lines = In_channel.input_lines stdin

let part1 () = 0
let part2 () = 0

let () =
  Prelude.Runner.run part1 part2;
  printf "Execution time: %.3f ms
" (sw ())
