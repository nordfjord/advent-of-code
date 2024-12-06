open Base
open Stdio

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]

  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
end

module Move = struct
  type t = Point.t * Point.t [@@deriving sexp, compare, hash, equal]
end

let lines = In_channel.input_lines stdin |> List.to_array

let initial_loc =
  let rec aux i j =
    if i = Array.length lines
    then (i - 1, j)
    else if j = String.length lines.(0)
    then aux (i + 1) 0
    else if Char.equal lines.(i).[j] '^'
    then (i, j)
    else aux i (j + 1)
  in
  aux 0 0

let max_x = Array.length lines
let max_y = String.length lines.(0)
let move = Point.add
let is_end (x, y) = x = -1 || x = max_x || y = -1 || y = max_y
let resolve (x, y) = lines.(x).[y]
let is_wall (x, y) = Char.equal lines.(x).[y] '#'

let rotate = function
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> failwith "invalid direction"

let part1 () =
  let visited = Hash_set.create (module Point) in
  Hash_set.add visited initial_loc;
  let rec aux loc dir =
    let next = move loc dir in
    if is_end next
    then ()
    else if is_wall next
    then aux loc (rotate dir)
    else (
      Hash_set.add visited next;
      aux next dir)
  in
  aux initial_loc (-1, 0);
  visited

let has_loop obstr =
  let moves = Hash_set.create (module Move) in
  let move loc dir =
    let rec aux curr =
      let next = Point.add curr dir in
      if is_end next
      then (loc, next)
      else if is_wall next || Point.equal obstr next
      then (loc, curr)
      else aux next
    in
    aux loc
  in
  let rec aux loc dir =
    let move = move loc dir in
    let _, next = move in
    (* We've made this move before, ergo loop *)
    if Hash_set.mem moves move
    then true
    else if is_end next
    then false
    else (
      Hash_set.add moves move;
      aux next (rotate dir))
  in
  aux initial_loc (-1, 0)

let part2 path =
  let obstructions = Hash_set.create (module Point) in
  Hash_set.iter path ~f:(fun x -> if has_loop x then Hash_set.add obstructions x);
  Hash_set.length obstructions

let () =
  let visited = part1 () in
  Hash_set.length visited |> printf "Part 1: %d\n";
  part2 visited |> printf "Part 2: %d\n"
