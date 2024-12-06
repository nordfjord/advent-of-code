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
let is_end (x, y) = x = -1 || x = max_x || y = -1 || y = max_y
let is_wall (x, y) = Char.equal lines.(x).[y] '#'

let rotate = function
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> failwith "invalid direction"

let move ~is_wall loc dir =
  let rec aux curr =
    let next = Point.add curr dir in
    if is_end next then (loc, next) else if is_wall next then (loc, curr) else aux next
  in
  aux loc

let expand ((x1, y1), (x2, y2)) =
  let x1, x2 = (min x1 x2, max x1 x2) in
  let y1, y2 = (min y1 y2, max y1 y2) in
  (if x1 = x2
   then Sequence.range ~stop:`inclusive y1 y2 |> Sequence.map ~f:(fun y -> (x1, y))
   else Sequence.range ~stop:`inclusive x1 x2 |> Sequence.map ~f:(fun x -> (x, y1)))
  |> Sequence.filter ~f:(Fn.non is_end)

let part1 () =
  let visited = Hash_set.create (module Point) ~size:128 in
  Hash_set.add visited initial_loc;
  let rec aux loc dir =
    let move = move ~is_wall loc dir in
    let next = snd move in
    Sequence.iter (expand move) ~f:(fun x -> Hash_set.add visited x);
    if is_end next then visited else aux next (rotate dir)
  in
  aux initial_loc (-1, 0)

let has_loop obstr =
  let is_wall_or_obstr x = is_wall x || Point.equal x obstr in
  let moves = Hash_set.create (module Move) in
  let rec aux loc dir =
    let move = move ~is_wall:is_wall_or_obstr loc dir in
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

let part2 path = Hash_set.filter path ~f:has_loop |> Hash_set.length

let () =
  let visited = part1 () in
  Hash_set.length visited |> printf "Part 1: %d\n";
  part2 visited |> printf "Part 2: %d\n"
