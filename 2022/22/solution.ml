open Printf

let lines =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None)

type block = Air | Floor | Wall

let parse_block = function
  | ' ' -> Air
  | '#' -> Wall
  | '.' -> Floor
  | _ -> failwith "Nope"

let raw_map = lines |> Seq.take_while (fun l -> l <> "") |> Array.of_seq

let rows, cols =
  ( Array.length raw_map,
    raw_map |> Array.map String.length |> Array.fold_left max 0 )

let map =
  raw_map
  |> Array.map (fun s ->
         let len = String.length s in
         if len < cols then s ^ String.make (cols - len) ' ' else s)
  |> Array.map (fun s -> String.to_seq s |> Seq.map parse_block |> Array.of_seq)

let starting_position =
  let pos = ref 0 in
  while map.(0).(!pos) <> Floor do
    pos := !pos + 1
  done;
  (0, !pos)

type heading = Up | Down | Left | Right [@@deriving show]
type move = Move of int | RotateRight | RotateLeft

let rotate_r = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let rotate_l = function
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up

let modulo n m = ((n mod m) + m) mod m

let move (row, col) = function
  | Up -> (modulo (row - 1) rows, col)
  | Down -> (modulo (row + 1) rows, col)
  | Left -> (row, modulo (col - 1) cols)
  | Right -> (row, modulo (col + 1) cols)

let position map (row, col) = map.(row).(col)

let rec advance map pos heading n =
  if n = 0 then pos
  else
    let new_pos = ref (move pos heading) in
    while position map !new_pos = Air do
      new_pos := move !new_pos heading
    done;
    match position map !new_pos with
    | Air -> failwith "Impossible Air"
    | Wall -> pos
    | Floor -> advance map !new_pos heading (n - 1)

let matched_group_opt n s =
  match Str.matched_group n s with x -> Some x | exception Not_found -> None

let parse_input line =
  let re = Str.regexp {|\([0-9]+\)\(R\|L\)?|} in
  let pos = ref 0 in
  let result = ref [||] in
  while Str.string_match re line !pos do
    let moves =
      match (matched_group_opt 1 line, matched_group_opt 2 line) with
      | Some n, Some "R" -> [| Move (int_of_string n); RotateRight |]
      | Some n, Some "L" -> [| Move (int_of_string n); RotateLeft |]
      | Some n, None -> [| Move (int_of_string n) |]
      | _ -> failwith "Unable to parse"
    in
    result := Array.append !result moves;
    pos := !pos + String.length (Str.matched_string line)
  done;
  !result

let score row col heading =
  (1000 * (row + 1))
  + (4 * (col + 1))
  + match heading with Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3

let () =
  let moves = parse_input (read_line ()) in

  let (row, col), heading =
    moves
    |> Array.fold_left
         (fun (position, heading) move ->
           match move with
           | Move n -> (advance map position heading n, heading)
           | RotateRight -> (position, rotate_r heading)
           | RotateLeft -> (position, rotate_l heading))
         (starting_position, Right)
  in

  printf "\n";
  printf "row=%d; col=%d; heading=%s\n" row col (show_heading heading);
  printf "Part 1: %d\n" (score row col heading)

let () =
  (* detect test input by row length *)
  let cube_size = if rows = 12 then 5 else 50 in
  printf "cube_size=%d\n" cube_size
