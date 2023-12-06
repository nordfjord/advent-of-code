open Printf

let numbers =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Seq.fold_left
       (fun state line ->
         match line with
         | "" -> 0 :: state
         | s -> (int_of_string s + List.hd state) :: List.tl state)
       [ 0 ]

let desc a b = -compare a b

let () =
  numbers |> List.fold_left max 0 |> printf "Part 1: %d\n";
  numbers
  |> List.sort desc
  |> List.to_seq
  |> Seq.take 3
  |> Seq.fold_left ( + ) 0
  |> printf "Part 2: %d\n"
