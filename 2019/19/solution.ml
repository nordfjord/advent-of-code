open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()
let line = In_channel.input_line stdin |> Option.value_exn
let computer = Computer.get_computer line

let check_beam (x, y) =
  let comp = Computer.copy computer in
  match Computer.run comp with
  | Computer.InputRequested cont ->
    (match cont x with
     | Computer.InputRequested cont ->
       (match cont y with
        | Computer.Out (output, _) -> output
        | _ -> failwith "Unexpected halt after second input")
     | _ -> failwith "Unexpected halt after input")
  | _ -> failwith "Unexpected halt before input"

let part1 () =
  Sequence.range 0 50
  |> Sequence.cartesian_product (Sequence.range 0 50)
  |> Sequence.sum (module Int) ~f:check_beam

module IntPair = struct
  type t = int * int [@@deriving hash, compare, sexp]
end

let part2 () =
  let find_x y =
    let rec binary_search low high =
      if low >= high
      then low
      else (
        let mid = (low + high) / 2 in
        if check_beam (mid, y) = 1
        then binary_search low mid
        else binary_search (mid + 1) high)
    in
    binary_search 0 (y * 2)
  in
  let check_fit (x, y) = check_beam (x, y) = 1 && check_beam (x + 99, y - 99) = 1 in
  let res =
    Sequence.range 100 10_000
    |> Sequence.find_map ~f:(fun y ->
      let x = find_x y in
      if check_fit (x, y) then Some (x, y - 99) else None)
  in
  match res with
  | Some (x, y) -> (x * 10000) + y
  | None -> 0

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
