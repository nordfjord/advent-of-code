open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse str =
  str |> String.to_array |> Array.map ~f:(fun x -> Char.to_int x - Char.to_int '0')

let solve k lst =
  let digits = Array.create ~len:k 0 in
  let rec aux (idx, v) sidx i arr k =
    let max_idx = Array.length arr - (k - 1) in
    if i >= Array.length digits
    then ()
    else if sidx >= max_idx
    then (
      digits.(i) <- v;
      aux (0, 0) (idx + 1) (i + 1) arr (k - 1))
    else if arr.(sidx) > v
    then aux (sidx, arr.(sidx)) (sidx + 1) i arr k
    else aux (idx, v) (sidx + 1) i arr k
  in
  aux (0, 0) 0 0 lst k;
  let num = Array.fold digits ~init:0 ~f:(fun acc x -> (acc * 10) + x) in
  num

let () =
  let batteries = List.map lines ~f:parse in
  batteries |> List.sum (module Int) ~f:(solve 2) |> printf "Part 1: %d\n%!";
  batteries |> List.sum (module Int) ~f:(solve 12) |> printf "Part 2: %d\n"
