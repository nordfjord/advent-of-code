let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> List.of_seq

let parse line = String.split_on_char ' ' line |> List.map int_of_string

let rec pairwise = function
  | x :: y :: tl -> (x, y) :: pairwise (y :: tl)
  | _ -> []

let extrapolate_one_level list = pairwise list |> List.map (fun (x, y) -> y - x)

let extrapolate list =
  let rec aux l list =
    let next = extrapolate_one_level list in
    if List.for_all (fun x -> x = 0) next
    then next :: l
    else aux (next :: l) next
  in
  aux [ list ] list

let last l = List.rev l |> List.hd
let determine_next = List.fold_left (fun acc xs -> acc + last xs) 0
let determine_previous = List.fold_left (fun acc xs -> List.hd xs - acc) 0
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
