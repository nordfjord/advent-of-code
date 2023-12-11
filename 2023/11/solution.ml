let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let find_all_planets arr =
  Array.to_seq arr
  |> Seq.mapi (fun row line ->
    line |> String.to_seq |> Seq.mapi (fun col c -> ((row, col), c)))
  |> Seq.concat
  |> Seq.filter_map (fun ((row, col), c) ->
    match c with
    | '#' -> Some (row, col)
    | _ -> None)

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let planet_distances planets =
  let planets = Array.to_seq planets in
  planets
  |> Seq.mapi (fun i planet ->
    planets
    |> Seq.drop (i + 1)
    |> Seq.map (manhattan_distance planet)
    |> Seq.fold_left ( + ) 0)
  |> Seq.fold_left ( + ) 0

let find_widening_points lines =
  let rows =
    Array.to_seq lines
    |> Seq.mapi (fun i line -> (i, line))
    |> Seq.filter (fun (_, line) -> String.for_all (( = ) '.') line)
    |> Seq.map fst
  in
  let cols =
    Seq.ints 0
    |> Seq.take (String.length lines.(0) - 1)
    |> Seq.filter (fun i ->
      Array.to_seq lines |> Seq.map (fun line -> line.[i]) |> Seq.for_all (( = ) '.'))
  in
  (rows, cols)

let rows, cols = find_widening_points lines

let adjust factor (x, y) =
  let intersecting_rows = Seq.filter (fun x' -> x' < x) rows |> Seq.length in
  let intersecting_cols = Seq.filter (fun y' -> y' < y) cols |> Seq.length in
  let x' = x + ((intersecting_rows * factor) - intersecting_rows) in
  let y' = y + ((intersecting_cols * factor) - intersecting_cols) in
  (x', y')

let planets = find_all_planets lines |> Array.of_seq

let () =
  let factor = 2 in
  planets |> Array.map (adjust factor) |> planet_distances |> Printf.printf "Part 1: %d\n"

let () =
  let factor = 1_000_000 in
  planets |> Array.map (adjust factor) |> planet_distances |> Printf.printf "Part 2: %d\n"
