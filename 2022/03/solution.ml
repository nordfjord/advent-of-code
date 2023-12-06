open Printf

let rec chunk size seq () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (_, _) -> Seq.Cons (Seq.take size seq, chunk size (Seq.drop size seq))

let head seq =
  match seq () with
  | Seq.Cons (x, _) -> x
  | _ -> raise Not_found

let parse_bag line =
  let length = String.length line in
  let firstCompartment = String.sub line 0 (length / 2) in
  let secondCompartment = String.sub line (length / 2) (length / 2) in
  (firstCompartment, secondCompartment)

module CharSet = Set.Make (Char)

let char_set s = s |> String.to_seq |> CharSet.of_seq

let find_error (first, second) =
  char_set first |> CharSet.inter (char_set second) |> CharSet.to_seq |> head

let a = int_of_char 'a'
let capA = int_of_char 'A'

let score_char (c : char) =
  let i = int_of_char c in
  if i >= a then i - a + 1 else i - capA + 27

let find_badge (first, second, third) =
  char_set first
  |> CharSet.inter (char_set second)
  |> CharSet.inter (char_set third)
  |> CharSet.to_seq
  |> head

let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let () =
  Array.to_seq lines
  |> Seq.map parse_bag
  |> Seq.map find_error
  |> Seq.map score_char
  |> Seq.fold_left ( + ) 0
  |> printf "Part 1: %d\n%!";
  Array.to_seq lines
  |> chunk 3
  |> Seq.filter_map (fun s ->
    match s |> List.of_seq with
    | a :: b :: c :: _ -> Some (a, b, c)
    | _ -> None)
  |> Seq.map find_badge
  |> Seq.map score_char
  |> Seq.fold_left ( + ) 0
  |> printf "Part 2: %d\n"
