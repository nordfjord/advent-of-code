open Base
open Stdio

let run p1 p2 =
  let p1, p1t = Performance.time p1 in
  let p2, p2t = Performance.time p2 in
  printf "Part 1: %d (%.3f ms)\n" p1 (Int63.to_float p1t /. 1_000_000.);
  printf "Part 2: %d (%.3f ms)\n" p2 (Int63.to_float p2t /. 1_000_000.)
