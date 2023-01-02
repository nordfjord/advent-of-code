type t = int * int

let compare = compare
let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let show (x, y) = Printf.sprintf "(%d, %d)" x y
let pi = 4.0 *. atan 1.0

let angle (x1, y1) (x2, y2) =
  let up = atan2 0. 1. in
  let x = x1 - x2 |> float_of_int in
  let y = y1 - y2 |> float_of_int in
  let result = up -. atan2 x y in
  if result < 0. then result +. (2. *. pi) else result
