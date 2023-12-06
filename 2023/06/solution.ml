let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let parse_numbers line = line |> Str.split (Str.regexp " +") |> List.map float_of_string

let parse lines =
  let [ _label; times ] = lines.(0) |> Str.split (Str.regexp ": ") in
  let [ _label; distances ] = lines.(1) |> Str.split (Str.regexp ": ") in
  let times = parse_numbers times in
  let distances = parse_numbers distances in
  Seq.zip (List.to_seq times) (List.to_seq distances) |> Array.of_seq
[@@warning "-8"]

let solve (time, distance) =
  let bound1 = (time +. sqrt ((time ** 2.) -. (4. *. distance))) /. 2. in
  let bound2 = (time -. sqrt ((time ** 2.) -. (4. *. distance))) /. 2. in
  int_of_float bound1 - int_of_float bound2

let part1 () =
  parse lines
  |> Array.fold_left (fun acc x -> acc * solve x) 1
  |> Printf.printf "Part 1: %d\n"

let parse_number line = line |> Str.global_replace (Str.regexp " +") "" |> float_of_string

let parse2 lines =
  let [ _label; times ] = lines.(0) |> Str.split (Str.regexp ": ") in
  let [ _label; distances ] = lines.(1) |> Str.split (Str.regexp ": ") in
  let time = parse_number times in
  let distance = parse_number distances in
  (time, distance)
[@@warning "-8"]

let part2 () = parse2 lines |> solve |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
