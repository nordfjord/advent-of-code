open Base
open Stdio

let measure name f =
  let now = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. now in
  printf "%s: %fms\n" name (elapsed /. 1000.);
  result

let time f =
  let start = Time_now.nanosecond_counter_for_timing () in
  let res = f () in
  let stop = Time_now.nanosecond_counter_for_timing () in
  let diff = Int63.(stop - start) in
  (res, diff)
