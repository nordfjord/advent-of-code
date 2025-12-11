open Base
open Stdio
module T = Domainslib.Task

let run p1 p2 =
  let p1, p1t = Performance.time p1 in
  let p2, p2t = Performance.time p2 in
  printf "Part 1: %d (%.3f ms)\n" p1 (Int63.to_float p1t /. 1_000_000.);
  printf "Part 2: %d (%.3f ms)\n" p2 (Int63.to_float p2t /. 1_000_000.)

let run_parallel p1 p2 =
  let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count ()) () in
  T.run pool (fun () ->
    let p1r = T.async pool (fun () -> Performance.time (fun _ -> p1 pool)) in
    let p2r = T.async pool (fun () -> Performance.time (fun _ -> p2 pool)) in
    let p1, p1t = T.await pool p1r in
    let p2, p2t = T.await pool p2r in
    printf "Part 1: %d (%.3f ms)\n" p1 (Int63.to_float p1t /. 1_000_000.);
    printf "Part 2: %d (%.3f ms)\n" p2 (Int63.to_float p2t /. 1_000_000.));
  T.teardown_pool pool
