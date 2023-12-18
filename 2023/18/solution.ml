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

  let of_string_p2 s =
    let i = String.index_exn s '(' in
    let j = String.index_exn s ')' in
    let n = String.subo s ~pos:(i + 2) ~len:(j - i - 3) in
    let n = Int.of_string @@ "0x" ^ n in
    let dir =
      match s.[String.length s - 2] with
      | '0' -> R
      | '1' -> D
      | '2' -> L
      | '3' -> U
      | _ -> failwith "invalid direction"
    in
    { dir; steps = n }

  let of_string s =
    Stdlib.Scanf.sscanf s "%c %d %s" (fun dir steps _color ->
      let dir =
        match dir with
        | 'R' -> R
        | 'L' -> L
        | 'U' -> U
        | 'D' -> D
        | _ -> failwith "invalid direction"
      in
      { dir; steps })
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
