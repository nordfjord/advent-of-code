let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> List.of_seq

let parse line = String.split_on_char ' ' line |> List.map int_of_string |> Array.of_list

let extrapolate_one_level s =
  let a = Array.make (Array.length s - 1) 0 in
  let has_non_zero = ref false in
  for i = 0 to Array.length s - 2 do
    let x, y = (s.(i), s.(i + 1)) in
    a.(i) <- y - x;
    if a.(i) <> 0 then has_non_zero := true
  done;
  (!has_non_zero, a)

let extent a = (a.(0), a.(Array.length a - 1))

let extrapolate arr =
  let rec aux l arr =
    let has_non_zero, next = extrapolate_one_level arr in
    if has_non_zero then aux (extent next :: l) next else extent next :: l
  in
  aux [ extent arr ] arr

let determine_next = List.fold_left (fun acc (_, x) -> acc + x) 0
let determine_previous = List.fold_left (fun acc (x, _) -> x - acc) 0
let extrapolated = lines |> List.map parse |> List.map extrapolate

let () =
  extrapolated
  |> List.map determine_next
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n"

let () =
  extrapolated
  |> List.map determine_previous
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"
