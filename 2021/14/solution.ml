open Base
open Stdio

let input = In_channel.input_lines stdin
let start = List.hd_exn input

let transformations =
  List.drop input 2
  |> List.map ~f:(fun s ->
    match Str.split (Str.regexp_string " -> ") s with
    | [ from; to_ ] -> (from, String.get to_ 0)
    | _ -> failwith "bad input")

let rec pairwise = function
  | [] | [ _ ] -> []
  | x :: (y :: _ as rest) -> (x, y) :: pairwise rest

let transform transformations (a, b) =
  List.find_map transformations ~f:(fun (from, c) ->
    if Char.(a = from.[0] && b = from.[1]) then Some c else None)

let update_count key count m =
  Map.update m key ~f:(function
    | None -> count
    | Some x -> x + count)

let apply_rules transformations input =
  input
  |> Map.fold ~init:Map.Poly.empty ~f:(fun ~key:(a, b) ~data:count m ->
    match transform transformations (a, b) with
    | Some c -> m |> update_count (a, c) count |> update_count (c, b) count
    | None -> m)

let rec apply_n n f x = if n = 0 then x else apply_n (n - 1) f (f x)

let extent m =
  m
  |> Map.fold
       ~init:(Map.empty (module Char))
       ~f:(fun ~key:(a, b) ~data:count m ->
         m |> update_count a count |> update_count b count)
  |> Map.fold
       ~init:(Int.max_value, Int.min_value)
       ~f:(fun ~key:_ ~data:count (min, max) -> (Int.min min count, Int.max max count))

(* each character is represented twice, so we need to divide by 2
   the 1 is for the ending character, e.g imagine
   the result is (N,C -> 1; C,N -> 1) and the string is
   NCN, we want the result to be 2 - 1 = 1.
   But we'd get 2 - 2 = 0 without the + 1 *)
let score m =
  let min, max = extent m in
  1 + ((max - min) / 2)

let () =
  let frequencies =
    String.to_list start
    |> pairwise
    |> List.fold ~init:Map.Poly.empty ~f:(fun m (a, b) ->
      Map.update m (a, b) ~f:(function
        | None -> 1
        | Some x -> x + 1))
  in
  apply_n 10 (apply_rules transformations) frequencies |> score |> printf "%d\n";
  apply_n 40 (apply_rules transformations) frequencies |> score |> printf "%d\n"
