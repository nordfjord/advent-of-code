open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.to_array
let resolve (x, y) = lines.(x).[y]
let max_x = Array.length lines - 1
let max_y = String.length lines.(0) - 1
let in_bounds (x, y) = 0 <= x && x <= max_x && 0 <= y && y <= max_y

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash]

  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
end

module PointDir = struct
  type t = Point.t * Point.t [@@deriving sexp, compare, hash]
end

let points_around p = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> List.map ~f:(Point.add p)

let moves (x, y) plant =
  points_around (x, y)
  |> List.filter ~f:(fun p -> in_bounds p && Char.equal (resolve p) plant)

let find_area (x, y) =
  let q = Queue.create () in
  Queue.enqueue q ((x, y), resolve (x, y));
  let visited = Hash_set.create (module Point) in
  Hash_set.add visited (x, y);
  while not (Queue.is_empty q) do
    let point, plant = Queue.dequeue_exn q in
    moves point plant
    |> List.iter ~f:(fun p ->
      if Hash_set.mem visited p
      then ()
      else (
        Hash_set.add visited p;
        Queue.enqueue q (p, resolve p)))
  done;
  visited

let gardens =
  let areas = ref [] in
  let visited = Hash_set.create (module Point) in
  for x = 0 to max_x do
    for y = 0 to max_y do
      if not (Hash_set.mem visited (x, y))
      then (
        let points = find_area (x, y) in
        areas := points :: !areas;
        Hash_set.iter points ~f:(Hash_set.add visited))
    done
  done;
  List.rev !areas

let perimeter points =
  let perimeter = Hash_set.create (module PointDir) in
  Hash_set.iter points ~f:(fun (x, y) ->
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> List.iter ~f:(fun (dx, dy) ->
      if not (Hash_set.mem points (x + dx, y + dy))
      then Hash_set.add perimeter ((x + dx, y + dy), (dx, dy))));
  perimeter

let sides points =
  let perimeter = perimeter points in
  let perimeter_arr = Hash_set.to_array perimeter in
  let visited = Hash_set.create (module PointDir) in
  let rec visit (dx, dy) (x, y) (sdx, sdy) =
    let x, y = (x + dx, y + dy) in
    if Hash_set.mem perimeter ((x, y), (sdx, sdy))
    then (
      Hash_set.add visited ((x, y), (sdx, sdy));
      visit (dx, dy) (x, y) (sdx, sdy))
  in
  Array.count perimeter_arr ~f:(fun x ->
    if not (Hash_set.mem visited x)
    then (
      Hash_set.add visited x;
      let (sx, sy), (sdx, sdy) = x in
      visit (sdy, sdx) (sx, sy) (sdx, sdy);
      visit (-sdy, -sdx) (sx, sy) (sdx, sdy);
      true)
    else false)

let price_perimeter area =
  let p = Hash_set.length @@ perimeter area in
  let a = Hash_set.length area in
  p * a

let price_sides area =
  let s = sides area in
  let a = Hash_set.length area in
  a * s

let part1 () = List.sum (module Int) gardens ~f:price_perimeter
let part2 () = List.sum (module Int) gardens ~f:price_sides

let () =
  part1 () |> printf "Part 1: %d\n";
  part2 () |> printf "Part 2: %d\n"
