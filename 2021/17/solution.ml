open Base
open Stdio

let line = In_channel.input_all stdin

let target_area =
  Stdlib.Scanf.sscanf line "target area: x=%d..%d, y=%d..%d" (fun x1 x2 y1 y2 ->
    ((x1, x2), (y1, y2)))

let is_within ((x1, x2), (y1, y2)) (x, y) =
  let x1, x2 = (min x1 x2, max x1 x2) in
  let y1, y2 = (min y1 y2, max y1 y2) in
  x1 <= x && x <= x2 && y1 <= y && y <= y2

let is_below (_, (y1, y2)) (_, y) = y < min y1 y2
let is_east ((x1, x2), _) (x, _) = x > max x1 x2

let () =
  printf
    "x1=%d x2=%d y1=%d y2=%d\n"
    (fst (fst target_area))
    (snd (fst target_area))
    (fst (snd target_area))
    (snd (snd target_area))

module IntTuple = struct
  type t = int * int [@@deriving sexp, compare, hash]

  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let apply_drag (x, y) : t =
    let x = if x < 0 then Int.(x + 1) else if x > 0 then x - 1 else 0 in
    (x, y - 1)
end

let simulate pos velocity =
  let rec aux pos velocity max_y =
    if is_within target_area pos
    then Some max_y
    else if is_east target_area pos || is_below target_area pos
    then None
    else
      IntTuple.(aux (pos + velocity) (IntTuple.apply_drag velocity) (max max_y (snd pos)))
  in
  aux pos velocity (snd pos)

let () =
  let count, max_y =
    Sequence.cartesian_product (Sequence.range (-200) 200) (Sequence.range (-200) 200)
    |> Sequence.filter_map ~f:(simulate (0, 0))
    |> Sequence.fold ~init:(0, Int.min_value) ~f:(fun (count, max_y) max_y' ->
      (count + 1, max max_y max_y'))
  in
  printf "%d\n" max_y;
  printf "%d\n" count
