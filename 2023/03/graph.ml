type node =
  | Num of int
  | Symbol of char

module IntIntSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let rows = Array.length lines
let cols = String.length lines.(0)
let is_digit c = '0' <= c && c <= '9'
let char_to_int c = int_of_char c - int_of_char '0'
let nodes = Hashtbl.create 100
let links = Hashtbl.create 100

let adjacent (x, y) =
  let diff = Seq.ints (-1) |> Seq.take 3 in
  diff
  |> Seq.concat_map (fun dx -> diff |> Seq.map (fun dy -> (x + dx, y + dy)))
  |> Seq.filter (fun (x, y) -> 0 <= x && x < cols && 0 <= y && y < rows)

let add_link k adjacent =
  match Hashtbl.find_opt links k with
  | None -> Hashtbl.add links k (IntIntSet.singleton adjacent)
  | Some set -> Hashtbl.replace links k (IntIntSet.add adjacent set)

let parse_line y str =
  let rec aux i =
    if i >= String.length str
    then ()
    else (
      match str.[i] with
      | '.' -> aux (i + 1)
      | c when is_digit c ->
        let key = (i, y) in
        let num = ref 0 in
        let i' = ref i in
        let coords = ref [] in
        while !i' < cols && is_digit str.[!i'] do
          num := (!num * 10) + char_to_int str.[!i'];
          coords := (!i', y) :: !coords;
          adjacent (!i', y)
          |> Seq.iter (fun k ->
               add_link k key;
               add_link key k);
          i' := !i' + 1
        done;
        let value = Num !num in
        Hashtbl.add nodes key value;
        aux !i'
      | c ->
        let key = (i, y) in
        let value = Symbol c in
        Hashtbl.add nodes key value;
        aux (i + 1))
  in
  aux 0

let () = lines |> Array.iteri parse_line

let find_adjacent_nodes (x, y) =
  match Hashtbl.find_opt links (x, y) with
  | None -> Seq.empty
  | Some set ->
    set
    |> IntIntSet.to_seq
    |> Seq.filter_map (fun (x, y) -> Hashtbl.find_opt nodes (x, y))

let has_adjacent_symbol (x, y) =
  find_adjacent_nodes (x, y)
  |> Seq.exists (function
       | Symbol _ -> true
       | _ -> false)

let part1 () =
  Hashtbl.to_seq nodes
  |> Seq.fold_left
       (fun acc (key, value) ->
         match value with
         | Num n when has_adjacent_symbol key -> n + acc
         | _ -> acc)
       0
  |> Printf.printf "Part 1: %d\n"

let calculate_gear_ratio (x, y) =
  find_adjacent_nodes (x, y)
  |> Seq.filter_map (function
       | Num n -> Some n
       | _ -> None)
  |> List.of_seq
  |> function
  | [ a; b ] -> Some (a * b)
  | _ -> None

let part2 () =
  Hashtbl.to_seq nodes
  |> Seq.filter_map (fun (key, value) ->
       match value with
       | Symbol '*' -> Some key
       | _ -> None)
  |> Seq.filter_map calculate_gear_ratio
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
