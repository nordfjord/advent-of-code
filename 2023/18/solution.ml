open Base
open Stdio

module Instruction = struct
  type dir =
    | R
    | L
    | U
    | D

  type t =
    { dir : dir
    ; steps : int
    }

  let dir t = t.dir
  let steps t = t.steps

  module Parse = struct
    open Angstrom

    let dir =
      choice
        [ char 'R' *> return R
        ; char 'L' *> return L
        ; char 'U' *> return U
        ; char 'D' *> return D
        ]

    let ws =
      skip_while (function
        | ' ' -> true
        | _ -> false)

    let int =
      take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
      >>| Int.of_string

    let part1 =
      let* dir = dir <* ws in
      let* steps = int in
      return { dir; steps }

    let hex_steps = take 5 >>| fun s -> Int.of_string @@ "0x" ^ s

    let dir =
      choice
        [ char '0' *> return R
        ; char '1' *> return D
        ; char '2' *> return L
        ; char '3' *> return U
        ]

    let part2 =
      let* _ = take_till (Char.equal '#') *> char '#' in
      let* steps = hex_steps in
      let* dir = dir in
      return { dir; steps }
  end

  let of_string_p2 s =
    Angstrom.parse_string ~consume:Prefix Parse.part2 s |> Result.ok_or_failwith

  let of_string s =
    Angstrom.parse_string ~consume:Prefix Parse.part1 s |> Result.ok_or_failwith
end

let lines = In_channel.input_lines stdin

module Coord = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

let points instructions =
  let grid = Queue.create () in
  let rec execute (x, y) instructions =
    match instructions with
    | [] -> grid
    | instr :: rest ->
      let next =
        match Instruction.dir instr with
        | R -> (x, y + instr.steps)
        | L -> (x, y - instr.steps)
        | U -> (x - instr.steps, y)
        | D -> (x + instr.steps, y)
      in
      Queue.enqueue grid next;
      execute next rest
  in
  execute (0, 0) instructions |> Queue.to_list

let shoelace_area vertices =
  let n = Array.length vertices in
  let rec aux acc i =
    if i = n
    then acc / 2
    else (
      let x1, y1 = vertices.(i) in
      let x2, y2 = vertices.((i + 1) % n) in
      let n = (x1 * y2) - (x2 * y1) in
      aux (acc + n) (i + 1))
  in
  abs (aux 0 0)

let solve instructions =
  let points = points instructions in
  let perimeter = instructions |> List.sum (module Int) ~f:Instruction.steps in
  let area = shoelace_area (Array.of_list points) in
  1 + (perimeter / 2) + area

let () =
  let part1 = List.map lines ~f:Instruction.of_string in
  let part2 = List.map lines ~f:Instruction.of_string_p2 in
  solve part1 |> printf "%d\n";
  solve part2 |> printf "%d\n"
