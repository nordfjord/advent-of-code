open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.to_array

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]
end

module IntIntList = struct
  type t = Point.t list [@@deriving sexp, compare, hash, equal]
end

let mapping =
  let tbl = Hashtbl.create (module Char) in
  lines
  |> Array.iteri ~f:(fun i s ->
    String.iteri s ~f:(fun j c ->
      match c with
      | '.' -> ()
      | c -> Hashtbl.add_multi tbl ~key:c ~data:(i, j)));
  tbl

let max_x = Array.length lines - 1
let max_y = String.length lines.(0) - 1
let in_bounds (x, y) = x >= 0 && x <= max_x && y >= 0 && y <= max_y

let find_anodes coords =
  List.cartesian_product coords coords
  |> List.concat_map ~f:(fun ((x1, y1), (x2, y2)) ->
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    let not_origin p = (not (Point.equal p (x1, y1))) && not (Point.equal p (x2, y2)) in
    [ (x1 - dx, y1 - dy); (x2 + dx, y2 + dy) ]
    |> List.filter ~f:(fun p -> not_origin p && in_bounds p))

let expand (x1, y1) (dx, dy) =
  if dx = 0 && dy = 0
  then Sequence.empty
  else
    Sequence.unfold ~init:(x1, y1) ~f:(fun (x1, y1) ->
      let next = (x1 + dx, y1 + dy) in
      if in_bounds next then Some (next, next) else None)

let anodes_expanded coords =
  let cs = Sequence.of_list coords in
  Sequence.cartesian_product cs cs
  |> Sequence.concat_map ~f:(fun ((x1, y1), (x2, y2)) ->
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    expand (x1, y1) (dx, dy))

let part1 () =
  let anodes = Hash_set.create (module Point) in
  Hashtbl.iter mapping ~f:(fun coords ->
    find_anodes coords |> List.iter ~f:(Hash_set.add anodes));
  anodes |> Hash_set.length

let part2 () =
  let anodes = Hash_set.create (module Point) in
  Hashtbl.iter mapping ~f:(fun coords ->
    anodes_expanded coords |> Sequence.iter ~f:(Hash_set.add anodes));
  anodes |> Hash_set.length

let () =
  part1 () |> printf "Part 1: %d\n%!";
  part2 () |> printf "Part 2: %d\n"
