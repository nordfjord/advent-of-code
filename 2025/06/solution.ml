open Base
open Stdio

let lines = In_channel.input_lines stdin
let parse_line line = Str.split (Str.regexp " +") line
let mul = List.fold ~init:1 ~f:( * )
let add = List.fold ~init:0 ~f:( + )

let part1 () =
  let tpose = lines |> List.map ~f:parse_line |> List.transpose_exn in
  let solve_col col =
    let rec aux (op, nums) col =
      match col with
      | [] -> op nums
      | "*" :: rest -> aux (mul, nums) rest
      | "+" :: rest -> aux (add, nums) rest
      | num :: rest -> aux (op, Int.of_string num :: nums) rest
    in
    aux (add, []) col
  in
  tpose |> List.sum (module Int) ~f:solve_col

let part2 () =
  let length = String.length (List.hd_exn lines) in
  let arr =
    List.map lines ~f:(fun s ->
      String.pad_right s ~char:' ' ~len:length |> String.to_list)
  in
  let cols =
    List.transpose_exn arr |> List.map ~f:(fun s -> String.of_list s |> String.strip)
  in
  let parse_op_num s = Str.global_replace (Str.regexp "[*+ ]") "" s |> Int.of_string in
  let rec aux (op, nums) col acc =
    match col with
    (* on end, evaluate current *)
    | [] -> acc + op nums
    (* on empty, evaluate current and reset *)
    | "" :: rest ->
      let res = op nums in
      aux (add, []) rest (acc + res)
    | s :: rest when String.contains s '*' -> aux (mul, parse_op_num s :: nums) rest acc
    | s :: rest when String.contains s '+' -> aux (add, parse_op_num s :: nums) rest acc
    | s :: rest -> aux (op, Int.of_string s :: nums) rest acc
  in
  aux (add, []) cols 0

let () =
  part1 () |> printf "Part 1: %d\n";
  part2 () |> printf "Part 2: %d\n"
