open Base
open Stdio

let lines = In_channel.input_lines stdin
let tiles = List.map lines ~f:(fun s -> Stdlib.Scanf.sscanf s "%d,%d" (fun x y -> (x, y)))

(* Takes coordinates of two edges of a rectangle and returns its area *)
let area ((x1, y1), (x2, y2)) = (1 + Int.abs (x2 - x1)) * (1 + Int.abs (y2 - y1))

let rec all_pairs = function
  | [] -> []
  | x :: xs -> List.map xs ~f:(fun y -> (x, y)) @ all_pairs xs

let pairwise l =
  if List.is_empty l then failwith "Empty list";
  let first_x = List.hd_exn l in
  let rec aux = function
    | x :: y :: xs -> (x, y) :: aux (y :: xs)
    | x :: [] -> [ (x, first_x) ]
    | [] -> []
  in
  aux l

let intersects edges ((x1, y1), (x2, y2)) =
  List.exists edges ~f:(fun ((ex1, ey1), (ex2, ey2)) ->
    x1 < ex2 && x2 > ex1 && y1 < ey2 && y2 > ey1)

let part1 tiles = all_pairs tiles |> List.map ~f:area |> List.fold ~init:0 ~f:Int.max

let part2 tiles =
  let edges =
    pairwise tiles
    |> List.map ~f:(fun ((x1, y1), (x2, y2)) ->
      let x1, x2 = (Int.min x1 x2, Int.max x1 x2) in
      let y1, y2 = (Int.min y1 y2, Int.max y1 y2) in
      ((x1, y1), (x2, y2)))
  in
  let rectangles = all_pairs tiles in
  List.filter rectangles ~f:(fun ((x1, y1), (x2, y2)) ->
    let x1, x2 = (Int.min x1 x2, Int.max x1 x2) in
    let y1, y2 = (Int.min y1 y2, Int.max y1 y2) in
    not @@ intersects edges ((x1, y1), (x2, y2)))
  |> List.map ~f:area
  |> List.fold ~init:0 ~f:Int.max

let () = part1 tiles |> printf "Part 1: %d\n"
let () = part2 tiles |> printf "Part 2: %d\n"
