open Base
open Stdio

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]

  let up = (-1, 0)
  let down = (1, 0)
  let left = (0, -1)
  let right = (0, 1)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let opp (x, y) = (-x, -y)
  let mul m (x, y) = (m * x, m * y)
end

module Tile = struct
  type t =
    | Wall
    | Box
    | Empty
  [@@deriving sexp, compare, hash, equal]
end

module Tile2 = struct
  type t =
    | Wall
    | LBox
    | RBox
    | Empty
  [@@deriving sexp, compare, hash, equal]

  let invert = function
    | LBox -> RBox
    | RBox -> LBox
    | x -> x
end

let boxes, start, instructions, xmax, ymax =
  let input = In_channel.input_all stdin in
  let [ map; instructions ] = Str.split (Str.regexp_string "\n\n") input in
  let instructions = Str.global_replace (Str.regexp "\n") "" instructions in
  let start = ref (0, 0) in
  let lines =
    String.split_lines map
    |> List.to_array
    |> Array.mapi ~f:(fun x s ->
      String.to_array s
      |> Array.mapi ~f:(fun y ->
          function
          | '#' -> Tile.Wall
          | 'O' -> Box
          | '@' ->
            start := (x, y);
            Empty
          | _ -> Empty))
  in
  (lines, !start, instructions, Array.length lines.(0) - 1, Array.length lines - 1)
[@@warning "-8"]

let in_bounds (x, y) = 0 <= x && x <= xmax && 0 <= y && y <= ymax
let resolve tiles (x, y) = tiles.(x).(y)

let instr_to_point instr =
  match instr with
  | '>' -> Point.right
  | '<' -> Point.left
  | '^' -> Point.up
  | 'v' -> Point.down
  | _ -> failwith "illegal instruction"

let move_boxes tiles start dir =
  let next = Point.add start dir in
  match resolve tiles next with
  | Tile.Box ->
    let box = ref next in
    while Tile.equal (resolve tiles !box) Box do
      box := Point.add dir !box
    done;
    if Tile.equal (resolve tiles !box) Wall
    then start
    else (
      let startx, starty = next in
      let endx, endy = !box in
      tiles.(startx).(starty) <- Empty;
      tiles.(endx).(endy) <- Box;
      next)
  | Tile.Wall -> start
  | Tile.Empty -> next

let score boxes =
  Array.foldi boxes ~init:0 ~f:(fun x sum xs ->
    sum
    + Array.foldi xs ~init:0 ~f:(fun y sum b ->
      match b with
      | Tile.Box -> sum + ((100 * x) + y)
      | _ -> sum))

let print_map boxes start =
  for x = 0 to Array.length boxes - 1 do
    for y = 0 to Array.length boxes.(x) - 1 do
      if Point.equal (x, y) start
      then printf "@"
      else (
        match boxes.(x).(y) with
        | Tile.Wall -> printf "#"
        | Box -> printf "O"
        | Empty -> printf ".")
    done;
    printf "\n"
  done;
  printf "\n"

let () =
  print_map boxes start;
  printf "%s\n" instructions

let rec part1 boxes start instrs =
  match instrs with
  | [] -> score boxes
  | instr :: instrs ->
    let dir = instr_to_point instr in
    let next = move_boxes boxes start dir in
    part1 boxes next instrs

let find_connected_boxes tiles (x, y) dir =
  let found = Hash_set.create (module Point) in
  let rec aux (x, y) =
    if Hash_set.mem found (x, y)
    then true
    else (
      match resolve tiles (x, y) with
      | Tile2.LBox ->
        Hash_set.add found (x, y);
        aux (Point.add dir (x, y)) && aux (Point.add dir (x, y + 1))
      | Tile2.RBox -> aux (x, y - 1)
      | Tile2.Wall -> false
      | _ -> true)
  in
  let has_space = aux (x, y) in
  (has_space, found |> Hash_set.to_list)

let move_boxes2 tiles start dir =
  let is_box x =
    match resolve tiles x with
    | Tile2.LBox | Tile2.RBox -> true
    | _ -> false
  in
  let next = Point.add start dir in
  match resolve tiles next with
  | (Tile2.LBox | Tile2.RBox)
    when Point.equal dir Point.left || Point.equal dir Point.right ->
    let box = ref next in
    while is_box !box do
      box := Point.add dir !box
    done;
    if Tile2.equal (resolve tiles !box) Wall
    then start
    else (
      let startx, starty = next in
      let endx, endy = !box in
      tiles.(startx).(starty) <- Empty;
      tiles.(endx).(endy) <- (if endy < starty then LBox else RBox);
      let negdir = Point.opp dir in
      while not (Point.equal next !box) do
        let px, py = Point.add negdir !box in
        tiles.(px).(py) <- Tile2.invert tiles.(px).(py);
        box := (px, py)
      done;
      next)
  | Tile2.LBox | Tile2.RBox ->
    let has_space, boxes = find_connected_boxes tiles next dir in
    if has_space
    then (
      List.iter boxes ~f:(fun (x, y) ->
        tiles.(x).(y) <- Empty;
        tiles.(x).(y + 1) <- Empty);
      List.iter boxes ~f:(fun (x, y) ->
        let x, y = Point.add dir (x, y) in
        tiles.(x).(y) <- LBox;
        tiles.(x).(y + 1) <- RBox);
      next)
    else start
  | Tile2.Wall -> start
  | Tile2.Empty -> next

let widen boxes =
  Array.map boxes ~f:(fun xs ->
    Array.concat_map xs ~f:(function
      | Tile.Wall -> [| Tile2.Wall; Wall |]
      | Box -> [| LBox; RBox |]
      | Empty -> [| Empty; Empty |]))

let score boxes =
  Array.foldi boxes ~init:0 ~f:(fun x sum xs ->
    sum
    + Array.foldi xs ~init:0 ~f:(fun y sum b ->
      match b with
      | Tile2.LBox -> sum + ((100 * x) + y)
      | _ -> sum))

let print_map boxes start =
  for x = 0 to Array.length boxes - 1 do
    for y = 0 to Array.length boxes.(x) - 1 do
      if Point.equal (x, y) start
      then printf "@"
      else (
        match boxes.(x).(y) with
        | Tile2.Wall -> printf "#"
        | LBox -> printf "["
        | RBox -> printf "]"
        | Empty -> printf ".")
    done;
    printf "\n"
  done;
  printf "\n"

let rec part2 boxes start instrs =
  match instrs with
  | [] -> score boxes
  | instr :: instrs ->
    let dir = instr_to_point instr in
    let next = move_boxes2 boxes start dir in
    printf "Move: %c\n" instr;
    print_map boxes next;
    part2 boxes next instrs

let () =
  let b = Array.copy_matrix boxes in
  part1 b start (String.to_list instructions) |> printf "Part 1: %d\n";
  let start = (fst start, 2 * snd start) in
  printf "%d,%d\n" (fst start) (snd start);
  print_map (widen boxes) start;
  part2 (widen boxes) start (String.to_list instructions) |> printf "Part 2: %d\n"
