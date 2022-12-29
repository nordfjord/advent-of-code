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

let moves = parse_input (read_line ())

let () =
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

let move (face, row, col, dir) =
  match dir with
  | Up -> (face, row - 1, col, dir)
  | Down -> (face, row + 1, col, dir)
  | Left -> (face, row, col - 1, dir)
  | Right -> (face, row, col + 1, dir)

type position = int * int * int * heading [@@deriving show]

let position map (face, row, col, _) = map.(face).(row).[col] |> parse_block

let side_mapping pos =
  match pos with
  (* Side 0 *)
  | 0, -1, col, Up -> (5, col, 0, Right)
  | 0, row, -1, Left -> (3, 49 - row, 0, Right)
  | 0, 50, col, Down -> (2, 0, col, Down)
  | 0, row, 50, Right -> (1, row, 0, Right)
  (* Side 1 *)
  | 1, -1, col, Up -> (5, 49, col, Up)
  | 1, row, -1, Left -> (0, row, 49, Left)
  | 1, 50, col, Down -> (2, col, 49, Left)
  | 1, row, 50, Right -> (4, 49 - row, 49, Left)
  (* Side 2 *)
  | 2, -1, col, Up -> (0, 49, col, Up)
  | 2, row, -1, Left -> (3, 0, row, Down)
  | 2, 50, col, Down -> (4, 0, col, Down)
  | 2, row, 50, Right -> (1, 49, row, Up)
  (* Side 3 *)
  | 3, -1, col, Up -> (2, col, 0, Right)
  | 3, row, -1, Left -> (0, 49 - row, 0, Right)
  | 3, 50, col, Down -> (5, 0, col, Down)
  | 3, row, 50, Right -> (4, row, 0, Right)
  (* Side 4 *)
  | 4, -1, col, Up -> (2, 49, col, Up)
  | 4, row, -1, Left -> (3, row, 49, Left)
  | 4, 50, col, Down -> (5, col, 49, Left)
  | 4, row, 50, Right -> (1, 49 - row, 49, Left)
  (* Side 5 *)
  | 5, -1, col, Up -> (3, 49, col, Up)
  | 5, row, -1, Left -> (0, 0, row, Down)
  | 5, 50, col, Down -> (1, 0, col, Down)
  | 5, row, 50, Right -> (4, 49, row, Up)
  (* No mapping necessary *)
  | _ -> pos

let rec advance map pos n =
  if n = 0 then pos
  else
    let new_pos = move pos |> side_mapping in
    match position map new_pos with
    | Air -> failwith ("Impossible Air at " ^ show_position new_pos)
    | Wall -> pos
    | Floor -> advance map new_pos (n - 1)

let get_all_sides () =
  let one_and_two = Array.sub raw_map 0 50 in
  let three = Array.sub raw_map 50 50 in
  let four_and_five = Array.sub raw_map 100 50 in
  let six = Array.sub raw_map 150 50 in
  let one = one_and_two |> Array.map (fun s -> String.sub s 50 50) in
  let two = one_and_two |> Array.map (fun s -> String.sub s 100 50) in
  let three = three |> Array.map (fun s -> String.sub s 50 50) in
  let four = four_and_five |> Array.map (fun s -> String.sub s 0 50) in
  let five = four_and_five |> Array.map (fun s -> String.sub s 50 50) in
  let six = six |> Array.map (fun s -> String.sub s 0 50) in
  [| one; two; three; four; five; six |]

let score face row col heading =
  let drow, dcol =
    match face with
    | 0 -> (0, 50)
    | 1 -> (0, 100)
    | 2 -> (50, 50)
    | 3 -> (100, 0)
    | 4 -> (100, 100)
    | 5 -> (150, 0)
    | _ -> failwith ("Unexpected side " ^ string_of_int face)
  in
  score (row + drow) (col + dcol) heading

let () =
  let map = get_all_sides () in

  let face, row, col, heading =
    moves
    |> Array.fold_left
         (fun (face, row, col, dir) move ->
           match move with
           | Move n -> advance map (face, row, col, dir) n
           | RotateRight -> (face, row, col, rotate_r dir)
           | RotateLeft -> (face, row, col, rotate_l dir))
         (0, 0, 0, Right)
  in

  printf "\n";
  printf "face=%d; row=%d; col=%d; heading=%s\n" face row col
    (show_heading heading);
  printf "Part 2: %d\n" (score face row col heading)
