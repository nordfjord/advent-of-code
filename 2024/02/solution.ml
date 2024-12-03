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
  Sequence.unfold ~init:([], xs) ~f:(function
    | _, [] -> None
    | acc, x :: xs ->
      let ys = List.rev_append acc xs in
      Some (ys, (x :: acc, xs)))

let ( >> ) f g x = g (f x)
let is_safe_with_one_removed nums = nums |> remove_one |> Sequence.exists ~f:is_safe
let part1 rows = rows |> List.sum (module Int) ~f:(is_safe >> Bool.to_int)
let part2 rows = rows |> List.sum (module Int) ~f:(is_safe_with_one_removed >> Bool.to_int)

let () =
  let nums = List.map lines ~f:(String.split ~on:' ' >> List.map ~f:Int.of_string) in
  part1 nums |> printf "Part 1: %d\n";
  part2 nums |> printf "Part 2: %d\n"
