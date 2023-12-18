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
    ; color : string
    }

  let dir t = t.dir
  let steps t = t.steps
  let color t = t.color

  let move t (x, y) =
    if t.steps = 0
    then None
    else (
      match t.dir with
      | R -> Some ((x, y + 1), { t with steps = t.steps - 1 })
      | L -> Some ((x, y - 1), { t with steps = t.steps - 1 })
      | U -> Some ((x - 1, y), { t with steps = t.steps - 1 })
      | D -> Some ((x + 1, y), { t with steps = t.steps - 1 }))

  let of_string s =
    Stdlib.Scanf.sscanf s "%c %d %s" (fun dir steps color ->
      let dir =
        match dir with
        | 'R' -> R
        | 'L' -> L
        | 'U' -> U
        | 'D' -> D
        | _ -> failwith "invalid direction"
      in
      { dir; steps; color })
end

let instructions = In_channel.input_lines stdin |> List.map ~f:Instruction.of_string

module Coord = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

let draw_bounds instructions =
  let grid = Hashtbl.create (module Coord) in
  let rec execute (x, y) instructions =
    match instructions with
    | [] -> grid
    | instr :: rest ->
      Hashtbl.set grid ~key:(x, y) ~data:();
      (match Instruction.move instr (x, y) with
       | None -> execute (x, y) rest
       | Some (c, instr) -> execute c (instr :: rest))
  in
  execute (0, 0) instructions

let print_arr a =
  Array.iter a ~f:(fun row ->
    Array.iter row ~f:(fun c -> printf "%c" c);
    printf "\n");
  printf "\n"

let flood_fill grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let rec fill (x, y) =
    if x < 0 || x >= rows || y < 0 || y >= cols
    then ()
    else if Char.(grid.(x).(y) = '.')
    then (
      grid.(x).(y) <- '#';
      fill (x + 1, y);
      fill (x - 1, y);
      fill (x, y + 1);
      fill (x, y - 1))
  in
  fill (0, 0)

let () =
  let tbl = draw_bounds instructions in
  let keys = tbl |> Hashtbl.keys in
  let min_x, max_x, min_y, max_y =
    List.fold
      keys
      ~init:(Int.max_value, Int.min_value, Int.max_value, Int.min_value)
      ~f:(fun (min_x, max_x, min_y, max_y) (x, y) ->
        (min min_x x, max max_x x, min min_y y, max max_y y))
  in
  (* shift x to 0 based *)
  let scale_x x = 1 + x - min_x in
  let scale_y y = 1 + y - min_y in
  let arr = Array.make_matrix ~dimx:(scale_x max_x + 2) ~dimy:(scale_y max_y + 2) '.' in
  keys |> List.iter ~f:(fun (x, y) -> arr.(scale_x x).(scale_y y) <- '#');
  flood_fill arr;
  let empty = Array.sum (module Int) arr ~f:(Array.count ~f:(Char.equal '.')) in
  let walls = keys |> List.length in
  empty + walls |> printf "%d\n"
