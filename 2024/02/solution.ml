open Base
open Stdio

let lines = In_channel.input_lines stdin
let check_desc a b = a > b && a - b <= 3
let check_asc a b = a < b && b - a <= 3

let is_safe = function
  | [ _ ] | [] -> false
  | a :: b :: row ->
    let rec aux check = function
      | [ _ ] | [] -> true
      | a :: b :: rest -> check a b && aux check (b :: rest)
    in
    if a = b
    then false
    else if check_desc a b
    then aux check_desc (b :: row)
    else if check_asc a b
    then aux check_asc (b :: row)
    else false

let remove_one xs =
  let open Sequence in
  let rec aux acc remaining =
    match remaining with
    | [] -> empty
    | x :: xs ->
      let ys = lazy (singleton (List.rev_append acc xs)) in
      append (of_lazy ys) (aux (x :: acc) xs)
  in
  aux [] xs

let is_safe_with_one_removed nums = nums |> remove_one |> Sequence.exists ~f:is_safe

let part1 rows =
  List.fold_left rows ~init:0 ~f:(fun sum xs -> sum + Bool.to_int (is_safe xs))

let part2 rows =
  List.fold_left rows ~init:0 ~f:(fun sum xs ->
    sum + Bool.to_int (is_safe_with_one_removed xs))

let () =
  let nums =
    lines |> List.map ~f:(Fn.compose (List.map ~f:Int.of_string) (String.split ~on:' '))
  in
  part1 nums |> printf "Part 1: %d\n";
  part2 nums |> printf "Part 2: %d\n"
