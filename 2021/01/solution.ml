open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.map ~f:Int.of_string

let part1 lines =
  let rec aux count lst =
    match lst with
    | a :: b :: xs when b > a -> aux (count + 1) (b :: xs)
    | _ :: b :: xs -> aux count (b :: xs)
    | [ _ ] | [] -> count
  in
  aux 0 lines

let part2 lines =
  let rec aux count lst =
    match lst with
    | a :: b :: c :: d :: rest ->
      let suma = a + b + c in
      let sumb = b + c + d in
      if sumb > suma
      then aux (count + 1) (b :: c :: d :: rest)
      else aux count (b :: c :: d :: rest)
    | [ _; _; _ ] | [ _; _ ] | [ _ ] | [] -> count
  in
  aux 0 lines

let () =
  part1 lines |> printf "Part 1: %d\n";
  part2 lines |> printf "Part 2: %d\n"
