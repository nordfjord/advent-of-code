open Printf
open Prelude

type move = Right of int | Left of int | Up of int | Down of int

let move_to_vector = function
  | Right _ -> (1, 0)
  | Left _ -> (-1, 0)
  | Up _ -> (0, 1)
  | Down _ -> (0, -1)

let move_to_magnitude = function
  | Right n -> n
  | Left n -> n
  | Up n -> n
  | Down n -> n

module PointSet = Set.Make (Point)

let parse_line line =
  line |> String.split_on_char ','
  |> List.map (fun s ->
         Scanf.sscanf s "%c%d" (fun dir amount ->
             match dir with
             | 'U' -> Up amount
             | 'R' -> Right amount
             | 'D' -> Down amount
             | 'L' -> Left amount
             | _ -> failwith "Invalid input" ) )

let move_list_to_point_list l =
  let coords = ref (0, 0) in
  l
  |> List.concat_map (fun move ->
         let vec = move_to_vector move in
         let magnitude = move_to_magnitude move in
         Seq.ints 0 |> Seq.take magnitude
         |> Seq.map (fun _ ->
                (coords := Point.(!coords + vec))
                ; !coords )
         |> List.of_seq )

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 + x2) + abs (y1 + y2)

let () =
  let lines =
    Aoc.stdin_seq () |> Seq.map parse_line
    |> Seq.map move_list_to_point_list
    |> Array.of_seq
  in
  let line1 = PointSet.of_list lines.(0) in
  let line2 = PointSet.of_list lines.(1) in
  let intersections = PointSet.inter line1 line2 |> PointSet.to_seq in
  intersections
  |> Seq.map (manhattan_distance (0, 0))
  |> Seq.fold_left min Int.max_int
  |> printf "Part 1: %d\n"
  ; intersections
    |> Seq.map (fun p ->
           let line1dist = List.index_of p lines.(0) in
           let line2dist = List.index_of p lines.(1) in
           (* Add the two back since we didn't include 0,0 as coordinates *)
           2 + line1dist + line2dist )
    |> Seq.fold_left min Int.max_int
    |> printf "Part 2: %d\n"
