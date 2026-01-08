open Base

let start () =
  let start = Time_now.nanosecond_counter_for_timing () in
  fun () ->
    let stop = Time_now.nanosecond_counter_for_timing () in
    let ms = Int63.(to_float (stop - start) /. 1_000_000.) in
    ms
