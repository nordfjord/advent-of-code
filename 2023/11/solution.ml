open Base
open Stdio

let lines = In_channel.input_lines stdin |> Array.of_list

let find_all_planets =
  Array.concat_mapi ~f:(fun row line ->
    line
    |> String.to_array
    |> Array.filter_mapi ~f:(fun col c ->
      match c with
      | '#' -> Some (row, col)
      | _ -> None))

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let planet_distances planets =
  let result = ref 0 in
  for i = 0 to Array.length planets - 1 do
    for j = i + 1 to Array.length planets - 1 do
      result := !result + manhattan_distance planets.(i) planets.(j)
    done
  done;
  !result

let find_widening_points lines =
  let rows =
    lines
    |> Array.to_sequence_mutable
    |> Sequence.filter_mapi ~f:(fun i line ->
      if String.for_all ~f:(Char.equal '.') line then Some i else None)
  in
  let cols =
    Sequence.range 0 (String.length lines.(0))
    |> Sequence.filter ~f:(fun i ->
      lines |> Array.for_all ~f:(fun line -> Char.equal line.[i] '.'))
  in
  (rows, cols)

let rows, cols = find_widening_points lines

let adjust factor (x, y) =
  let widened_rows = Sequence.count rows ~f:(Int.( > ) x) in
  let widened_cols = Sequence.count cols ~f:(Int.( > ) y) in
  let x' = x + ((widened_rows * factor) - widened_rows) in
  let y' = y + ((widened_cols * factor) - widened_cols) in
  (x', y')

let planets = find_all_planets lines

let () =
  let factor = 2 in
  planets |> Array.map ~f:(adjust factor) |> planet_distances |> printf "Part 1: %d\n"

let () =
  let factor = 1_000_000 in
  planets |> Array.map ~f:(adjust factor) |> planet_distances |> printf "Part 2: %d\n"
