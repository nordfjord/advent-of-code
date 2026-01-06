open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()
let lines = In_channel.input_all stdin

let locks_and_keys_diagrams =
  Str.split (Str.regexp_string "\n\n") lines
  |> List.map ~f:(fun s -> String.split_lines s |> List.to_array)

let locks_diagrams, keys_diagrams =
  List.partition_tf locks_and_keys_diagrams ~f:(fun diagram ->
    String.(diagram.(0) = "#####"))

let locks_heights =
  List.map locks_diagrams ~f:(fun xs ->
    List.range 0 5
    |> List.map ~f:(fun i -> Array.count xs ~f:(fun s -> Char.(s.[i] = '#')) - 1))

let keys_heights =
  List.map keys_diagrams ~f:(fun xs ->
    List.range 0 5
    |> List.map ~f:(fun i -> Array.count xs ~f:(fun s -> Char.(s.[i] = '#')) - 1))

let part1 () =
  List.sum
    (module Int)
    locks_heights
    ~f:(fun lock ->
      List.count keys_heights ~f:(fun key ->
        List.for_all2_exn lock key ~f:(fun lh kh -> lh + kh <= 5)))

let part2 () = 0

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
