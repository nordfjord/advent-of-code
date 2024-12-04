open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.map ~f:String.to_array |> Array.of_list
let ( >> ) f g x = g (f x)

module CharArrayArray = struct
  type t = char array array [@@deriving show]
end

let tap f x =
  f x;
  x

let check_xmas lines (i, j) =
  let max_i = Array.length lines in
  let max_j = Array.length lines.(0) in
  let valid_idx (i, j) = 0 <= i && i < max_i && 0 <= j && j < max_j in
  let resolve (i, j) = lines.(i).(j) in
  [ [ (i, j); (i + 1, j); (i + 2, j); (i + 3, j) ]
  ; [ (i, j); (i - 1, j); (i - 2, j); (i - 3, j) ]
  ; [ (i, j); (i, j + 1); (i, j + 2); (i, j + 3) ]
  ; [ (i, j); (i, j - 1); (i, j - 2); (i, j - 3) ]
  ; [ (i, j); (i + 1, j + 1); (i + 2, j + 2); (i + 3, j + 3) ]
  ; [ (i, j); (i + 1, j - 1); (i + 2, j - 2); (i + 3, j - 3) ]
  ; [ (i, j); (i - 1, j - 1); (i - 2, j - 2); (i - 3, j - 3) ]
  ; [ (i, j); (i - 1, j + 1); (i - 2, j + 2); (i - 3, j + 3) ]
  ]
  |> List.filter ~f:(List.for_all ~f:valid_idx)
  |> List.map ~f:(List.map ~f:resolve >> String.of_list)
  |> List.count ~f:(String.equal "XMAS")

let check_mas lines (i, j) =
  let max_i = Array.length lines in
  let max_j = Array.length lines.(0) in
  let valid_idx (i, j) = 0 <= i && i < max_i && 0 <= j && j < max_j in
  let resolve (i, j) = lines.(i).(j) in
  let strings =
    [ [ (i - 1, j - 1); (i, j); (i + 1, j + 1) ]
    ; [ (i - 1, j + 1); (i, j); (i + 1, j - 1) ]
    ]
    |> List.filter ~f:(List.for_all ~f:valid_idx)
    |> List.map ~f:(List.map ~f:resolve >> String.of_list)
  in
  (List.length strings = 2
   && List.for_all strings ~f:(fun s -> String.equal "MAS" s || String.equal "SAM" s))
  |> Bool.to_int

let part1 lines =
  Sequence.cartesian_product
    (Sequence.range 0 (Array.length lines))
    (Sequence.range 0 (Array.length lines.(0)))
  |> Sequence.sum (module Int) ~f:(check_xmas lines)

let part2 lines =
  Sequence.cartesian_product
    (Sequence.range 1 (Array.length lines))
    (Sequence.range 1 (Array.length lines.(0)))
  |> Sequence.sum (module Int) ~f:(check_mas lines)

let () =
  part1 lines |> printf "Part 1: %d\n";
  part2 lines |> printf "Part 2: %d\n"
