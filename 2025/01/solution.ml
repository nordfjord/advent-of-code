open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse_line str =
  let direction = String.get str 0 in
  let degrees = Int.of_string (String.sub str ~pos:1 ~len:(String.length str - 1)) in
  match direction with
  | 'L' -> -degrees
  | 'R' -> degrees
  | _ -> failwith "Invalid direction"

let simulate rotations =
  let rec aux z pos rotations =
    match rotations with
    | [] -> z
    | n :: rest ->
      let next = (pos + n) % 100 in
      let z = if next = 0 then z + 1 else z in
      aux z next rest
  in
  aux 0 50 rotations

let simulate_p2 rotations =
  let rec aux z pos rotations =
    match rotations with
    | [] -> z
    | n :: rest ->
      let next = pos + n in
      let crossings =
        if next >= 100
        then next / 100
        else if next <= 0 && pos = 0
        then -next / 100
        else if next <= 0
        then (-next / 100) + 1
        else 0
      in
      aux (z + crossings) (next % 100) rest
  in
  aux 0 50 rotations

let instr = lines |> List.map ~f:parse_line
let part1 () = simulate instr
let part2 () = simulate_p2 instr
let () = Prelude.Runner.run part1 part2
