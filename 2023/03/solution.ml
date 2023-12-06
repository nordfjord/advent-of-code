let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

type cell =
  | Number of int
  | Empty
  | Symbol of char

let is_digit c = '0' <= c && c <= '9'
let char_to_int c = int_of_char c - int_of_char '0'

(* The number of digits in a number is the floor of the log10 of the number + 1 *)
let digits n = int_of_float (log10 (float_of_int n)) + 1

let parse_number i str =
  let rec aux num i =
    if i >= String.length str
    then (i, num)
    else if is_digit str.[i]
    then aux ((num * 10) + char_to_int str.[i]) (i + 1)
    else (i, num)
  in
  aux 0 i

let parse_line y str =
  let rec aux i =
    if i >= String.length str
    then []
    else (
      match str.[i] with
      | '.' -> ((i, y), Empty) :: aux (i + 1)
      | c when is_digit c ->
        let next_i, num = parse_number i str in
        ((i, y), Number num) :: aux next_i
      | c -> ((i, y), Symbol c) :: aux (i + 1))
  in
  aux 0 |> List.to_seq

(* Example: ["123...#123"] *)
let parse_grid grid =
  grid |> Array.to_seq |> Seq.mapi (fun y line -> parse_line y line) |> Seq.concat

(* Consecutive digits make up a number *)

let gridlines = parse_grid lines
let grid = gridlines |> Hashtbl.of_seq

let adjacent (x, y) =
  [ (x - 1, y)
  ; (x + 1, y)
  ; (x, y - 1)
  ; (x, y + 1)
  ; (x - 1, y - 1)
  ; (x + 1, y + 1)
  ; (x - 1, y + 1)
  ; (x + 1, y - 1)
  ]

let has_adjacent_symbol (x, y) =
  adjacent (x, y)
  |> List.exists (fun (x, y) ->
    match Hashtbl.find_opt grid (x, y) with
    | Some (Symbol _) -> true
    | _ -> false)

let number_cells (x, y) n =
  let cell_length = digits n in
  Seq.ints 0 |> Seq.take cell_length |> Seq.map (fun i -> (x + i, y))

let number_has_adjacent_symbol (x, y) n =
  number_cells (x, y) n |> Seq.exists has_adjacent_symbol

let part1 () =
  grid
  |> Hashtbl.to_seq
  |> Seq.filter_map (fun ((x, y), cell) ->
    match cell with
    | Number n when number_has_adjacent_symbol (x, y) n -> Some n
    | _ -> None)
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n"

(* Part 2 *)
(* A reverse lookup for numbers to their cells *)
let numbers_lookup =
  grid
  |> Hashtbl.to_seq
  |> Seq.filter_map (fun ((x, y), cell) ->
    match cell with
    | Number n ->
      Some (number_cells (x, y) n |> Seq.map (fun (x', y') -> ((x', y'), (x, y))))
    | _ -> None)
  |> Seq.concat
  |> Hashtbl.of_seq

module IntIntSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

let adjacent_numbers (x, y) =
  let adjacent_cells = adjacent (x, y) in
  let adjacent_numbers =
    adjacent_cells
    |> List.filter_map (fun (x, y) -> Hashtbl.find_opt numbers_lookup (x, y))
    (* We may have multiple cells for the same number *)
    |> IntIntSet.of_list
    |> IntIntSet.to_seq
    |> Seq.filter_map (fun (x, y) ->
      match Hashtbl.find grid (x, y) with
      | Number n -> Some n
      | _ -> None)
    |> List.of_seq
  in
  (* Only gears with exactly two adjacent numbers are valid *)
  match adjacent_numbers with
  | [ a; b ] -> Some (a, b)
  | _ -> None

let part2 () =
  grid
  |> Hashtbl.to_seq
  |> Seq.filter_map (fun ((x, y), cell) ->
    match cell with
    | Symbol '*' ->
      (match adjacent_numbers (x, y) with
       | Some (a, b) -> Some (a * b)
       | None -> None)
    | _ -> None)
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
