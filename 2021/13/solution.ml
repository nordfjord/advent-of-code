open Base
open Stdio

let input = In_channel.input_all stdin

type fold =
  | X of int
  | Y of int
[@@deriving sexp, compare, hash, show]

module Parse = struct
  open Angstrom

  let int =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| Int.of_string

  let comma = char ','

  let point =
    let* x = int <* comma in
    let* y = int in
    return (x, y)

  let fold =
    string "fold along "
    *> choice
         [ (let+ n = string "x=" *> int in
            X n)
         ; (let+ n = string "y=" *> int in
            Y n)
         ]

  let parse =
    let* points = many1 (point <* end_of_line) <* end_of_line in
    let* folds = many1 (fold <* option () end_of_line) in
    return (points, folds)

  let of_string s = parse_string ~consume:All parse s
end

module T2Int = struct
  type t = int * int [@@deriving compare, hash, sexp, show]

  include Comparator.Make (struct
      type t = int * int [@@deriving compare, sexp]
    end)
end

let points, folds = Parse.of_string input |> Result.ok_or_failwith
let points = points |> Set.of_list (module T2Int)

let fold points fold =
  match fold with
  | X x ->
    points
    |> Set.map
         (module T2Int)
         ~f:(fun (x', y) -> if x' < x then (x', y) else ((2 * x) - x', y))
  | Y y ->
    points
    |> Set.map
         (module T2Int)
         ~f:(fun (x, y') -> if y' < y then (x, y') else (x, (2 * y) - y'))

let extent points =
  points |> Set.fold ~init:(0, 0) ~f:(fun (x, y) (x', y') -> (Int.max x x', Int.max y y'))

let print points =
  let x, y = extent points in
  for y' = 0 to y do
    for x' = 0 to x do
      if Set.mem points (x', y') then printf "#" else printf "."
    done;
    printf "\n"
  done

let () =
  List.hd_exn folds |> fold points |> Set.length |> printf "%d\n";
  let folded = folds |> List.fold ~init:points ~f:(fun points f -> fold points f) in
  print folded
