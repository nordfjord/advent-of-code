open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse_line line =
  match line |> Str.split (Str.regexp_string ": ") with
  | [ total; numbers ] ->
    let total = Int.of_string total in
    let numbers =
      numbers |> Str.split (Str.regexp_string " ") |> List.map ~f:Int.of_string
    in
    (total, numbers)
  | _ -> failwith "invalid line"

let equations = List.map lines ~f:parse_line

let rec permute_operators n ops =
  if n = 0
  then [ [] ]
  else (
    let prev = permute_operators (n - 1) ops in
    List.concat_map ops ~f:(fun op -> List.map prev ~f:(fun ops -> op :: ops)))

let solve operators (total, numbers) =
  let op_count = List.length numbers - 1 in
  let ops = permute_operators op_count operators in
  match numbers with
  | init :: rest ->
    List.exists ops ~f:(fun ops ->
      let rec aux total ops numbers =
        match (ops, numbers) with
        | op :: ops, a :: xs -> aux (op total a) ops xs
        | _ -> total
      in
      let tot = aux init ops rest in
      tot = total)
  | [] -> failwith "unexpected empty list"

let join x y =
  let rec num_digits n = if n = 0 then 0 else 1 + num_digits (n / 10) in
  let shift = num_digits y in
  (x * Int.pow 10 shift) + y

let part1 () =
  let solve = solve [ ( + ); ( * ) ] in
  List.filter equations ~f:solve |> List.sum (module Int) ~f:fst

let part2 () =
  let solve = solve [ ( + ); ( * ); join ] in
  List.filter equations ~f:solve |> List.sum (module Int) ~f:fst

let () =
  part1 () |> printf "Part 1: %d\n";
  part2 () |> printf "Part 2: %d\n"
