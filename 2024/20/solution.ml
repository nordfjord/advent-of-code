open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.map ~f:String.to_array |> List.to_array
let xmax = Array.length lines - 1
let ymax = Array.length lines.(0) - 1
let resolve (x, y) = lines.(x).(y)
let in_bounds (x, y) = 0 <= x && x <= xmax && 0 <= y && y <= ymax

module Point = struct
  type t = int * int [@@deriving sexp, hash, compare, equal]

  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
end

let start, end_ =
  let start, end_ = (ref (0, 0), ref (0, 0)) in
  for x = 0 to xmax do
    for y = 0 to ymax do
      let open Char in
      if resolve (x, y) = 'S' then start := (x, y);
      if resolve (x, y) = 'E' then end_ := (x, y)
    done
  done;
  (!start, !end_)

let neighbours (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let race start end_ =
  let q = Queue.create () in
  let dist = Hashtbl.create (module Point) in
  Queue.enqueue q (start, 0);
  Hashtbl.set dist ~key:start ~data:0;
  while not (Queue.is_empty q) do
    let start, cost = Queue.dequeue_exn q in
    if Point.equal start end_
    then ()
    else
      List.iter (neighbours start) ~f:(fun p ->
        if in_bounds p && (not (Hashtbl.mem dist p)) && not (Char.equal (resolve p) '#')
        then (
          Queue.enqueue q (p, cost + 1);
          Hashtbl.set dist ~key:p ~data:(cost + 1)))
  done;
  dist

let pairs a =
  let len = Array.length a in
  Sequence.range 0 len
  |> Sequence.concat_map ~f:(fun i ->
    Sequence.range (i + 1) len |> Sequence.map ~f:(fun j -> (a.(i), a.(j))))

let find_cheats dist points cheat_length =
  pairs points
  |> Sequence.count ~f:(fun (p1, p2) ->
    let distance = Point.dist p1 p2 in
    if 1 < distance && distance <= cheat_length
    then (
      match (Hashtbl.find dist p1, Hashtbl.find dist p2) with
      | Some a, Some b -> abs (a - b) - distance >= 100
      | _ -> false)
    else false)

let () =
  let dist = race start end_ in
  let points = Hashtbl.keys dist |> List.to_array in
  Array.sort points ~compare:Point.compare;
  find_cheats dist points 2 |> printf "Part 1: %d\n";
  find_cheats dist points 20 |> printf "Part 2: %d\n"
