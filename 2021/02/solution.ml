open Base
open Stdio

let lines = In_channel.input_lines stdin

module Command = struct
  type t =
    | Forward of int
    | Down of int
    | Up of int

  let parse str =
    match String.split str ~on:' ' with
    | [ "forward"; n ] -> Forward (Int.of_string n)
    | [ "down"; n ] -> Down (Int.of_string n)
    | [ "up"; n ] -> Up (Int.of_string n)
    | _ -> failwith "Invalid command"
end

let commands = List.map lines ~f:Command.parse
let score (h, d) = h * d

let part1 commands =
  let rec aux (h, d) cmds =
    match cmds with
    | [] -> score (h, d)
    | cmd :: rest ->
      (match cmd with
       | Command.Forward n -> aux (h + n, d) rest
       | Command.Down n -> aux (h, d + n) rest
       | Command.Up n -> aux (h, d - n) rest)
  in
  aux (0, 0) commands

let part2 commands =
  let rec aux (h, d, a) cmds =
    match cmds with
    | [] -> score (h, d)
    | cmd :: rest ->
      (match cmd with
       | Command.Down n -> aux (h, d, a + n) rest
       | Command.Up n -> aux (h, d, a - n) rest
       | Command.Forward n -> aux (h + n, d + (a * n), a) rest)
  in
  aux (0, 0, 0) commands

let () =
  part1 commands |> printf "Part 1: %d\n";
  part2 commands |> printf "Part 2: %d\n"
