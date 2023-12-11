let measure name f =
  let now = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. now in
  Printf.printf "%s: %fms\n" name (elapsed /. 1000.);
  result

