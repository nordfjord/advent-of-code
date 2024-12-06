open Base
open Stdio

let ( >> ) f g x = g (f x)

let tuple_of_list = function
  | [ a; b ] -> (a, b)
  | _ -> failwith "invalid input"

let rules, page_updates =
  In_channel.input_all stdin |> Str.split (Str.regexp_string "\n\n") |> tuple_of_list

let page_updates =
  page_updates
  |> String.split_lines
  |> List.map ~f:(String.split ~on:',' >> List.map ~f:Int.of_string >> Array.of_list)
  |> Array.of_list

let rules =
  rules
  |> String.split_lines
  |> List.map ~f:(String.split ~on:'|' >> List.map ~f:Int.of_string >> tuple_of_list)
  |> Array.of_list

let index_of item arr = Array.findi arr ~f:(fun _ x -> x = item) |> Option.map ~f:fst

let is_valid_order rules update =
  rules
  |> Array.for_all ~f:(fun (a, b) ->
    match (index_of a update, index_of b update) with
    | Some aidx, Some bidx -> aidx < bidx
    | _ -> true)

let rules_to_compare rules a b =
  match Array.find rules ~f:(fun (x, y) -> (x = a || x = b) && (y = a || y = b)) with
  | Some (x, y) when x = a && y = b -> 1
  | Some _ -> -1
  | None -> 0

let to_sorted_order rules update =
  update |> Array.sorted_copy ~compare:(rules_to_compare rules)

let middle_value arr = arr.(Array.length arr / 2)
let part1 rules update = if is_valid_order rules update then middle_value update else 0

let part2 rules update =
  if not @@ is_valid_order rules update
  then middle_value @@ to_sorted_order rules update
  else 0

let () =
  Array.sum (module Int) page_updates ~f:(part1 rules) |> printf "Part 1: %d\n";
  Array.sum (module Int) page_updates ~f:(part2 rules) |> printf "Part 2: %d\n"
