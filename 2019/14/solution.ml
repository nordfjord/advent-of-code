open Printf
open Prelude

type cost = (string * int) list [@@deriving show]
type line = cost * (string * int) [@@deriving show]
type linel = line list

type recipe =
  { amount : int
  ; name : string
  ; cost : (string * int) list
  }

module StrMap = Map.Make (String)

let costs =
  Aoc.stdin_seq ()
  |> Seq.map (fun s ->
       Scanf.sscanf s "%s@=> %d %s" (fun left amount t ->
         let cost =
           left
           |> Str.split (Str.regexp_string ", ")
           |> List.map (fun s -> Scanf.sscanf s "%d %s" (fun amount t -> (t, amount)))
         in
         { name = t; amount; cost }))
  |> List.of_seq

let tbl = Hashtbl.create 300
let () = costs |> List.iter (fun n -> Hashtbl.add tbl n.name n)

let merge =
  StrMap.merge (fun _ a b ->
    match (a, b) with
    | Some a, Some b -> if a + b = 0 then None else Some (a + b)
    | Some 0, None -> None
    | None, Some 0 -> None
    | Some a, None -> Some a
    | None, Some b -> Some b
    | _ -> None)

let add key value =
  StrMap.update key (function
    | None -> if value = 0 then None else Some value
    | Some x -> if x + value = 0 then None else Some (x + value))

(* Regular integer divison gives us a rounded-down number.
   In this exercise we need round-up semantics because otherwise we
   miss the remainder *)
let div_ceil a b = int_of_float (ceil (float_of_int a /. float_of_int b))

let rec components (node_type, amount) state =
  if node_type = "ORE"
  then state |> add "ORE" amount
  else (
    let owned = StrMap.find_opt node_type state |> Option.value ~default:0 in
    let node = Hashtbl.find tbl node_type in
    let per_creation = node.amount in
    let required = amount - owned in
    let multiplier = div_ceil required per_creation in
    let next_state =
      state |> add node_type (-amount) |> add node_type (per_creation * multiplier)
    in
    if required < 0
    then next_state
    else
      node.cost
      |> List.fold_left
           (fun state (name, amount) -> components (name, amount * multiplier) state)
           next_state)

let part1 () =
  components ("FUEL", 1) StrMap.empty |> StrMap.find "ORE" |> printf "Part 1: %d\n"

let () = part1 ()

let rec binary_search f target low high =
  if high = low
  then if f low > target then low - 1 else low
  else (
    let mid = (low + high) / 2 in
    if f mid > target
    then binary_search f target low (mid - 1)
    else if f mid < target
    then binary_search f target (mid + 1) high
    else mid)

let run count = components ("FUEL", count) StrMap.empty |> StrMap.find "ORE"
let part2 () = binary_search run 1_000_000_000_000 1 10_000_000 |> printf "Part 2: %d\n"
let () = part2 ()
