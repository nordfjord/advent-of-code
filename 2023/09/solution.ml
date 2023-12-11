open Prelude

let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> List.of_seq

let parse line = String.split_on_char ' ' line |> List.map int_of_string
let extrapolate_one_level s = List.pairwise s |> List.map (fun (x, y) -> y - x)
let extent l = (List.hd l, List.last l)

let extrapolate l =
  let rec aux l ls =
    let extrapolated = extrapolate_one_level l in
    let extent = extent l in
    if List.for_all (( = ) 0) extrapolated
    then extent :: ls
    else aux extrapolated (extent :: ls)
  in
  aux l []

let determine_next = List.fold_left (fun acc (_, x) -> acc + x) 0
let determine_previous = List.fold_left (fun acc (x, _) -> x - acc) 0
let extrapolated = lines |> List.map parse |> List.map extrapolate

let () =
  extrapolated
  |> List.map determine_next
  |> List.sum
  |> Printf.printf "Part 1: %d\n"

let () =
  extrapolated
  |> List.map determine_previous
  |> List.sum
  |> Printf.printf "Part 2: %d\n"
