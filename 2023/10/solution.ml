let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let rows = Array.length lines
let cols = String.length lines.(0)

let adjacent (row, col) =
  [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]

let find_start lines =
  lines
  |> Array.to_seqi
  |> Seq.find_map (fun (row, line) ->
    line
    |> String.to_seqi
    |> Seq.find_map (fun (col, c) ->
      match c with
      | 'S' -> Some (row, col)
      | _ -> None))
  |> Option.get

type intpair = int * int [@@deriving show]

let infer_pipe (row, col) =
  let resolve (row, col) = lines.(row).[col] in
  let adjacent =
    adjacent (row, col)
    |> List.filter (fun (row, col) -> 0 <= row && row < rows && 0 <= col && col < cols)
    |> List.map (fun x -> (x, resolve x))
  in
  let is_connected = function
    | (_row', col'), '|' -> col' = col
    | (row', _col'), '-' -> row' = row
    | (row', col'), 'L' -> (row' = row + 1 && col' = col) || (row' = row && col' = col - 1)
    | (row', col'), 'J' -> (row' = row + 1 && col' = col) || (row' = row && col' = col + 1)
    | (row', col'), '7' -> (row' = row - 1 && col' = col) || (row' = row && col' = col + 1)
    | (row', col'), 'F' -> (row' = row - 1 && col' = col) || (row' = row && col' = col - 1)
    | _ -> false
  in
  let connected =
    List.filter is_connected adjacent
    |> List.map (fun ((row', col'), _pipe) -> (row' - row, col' - col))
  in
  match connected with
  | [ (-1, 0); (1, 0) ] -> '|'
  | [ (0, -1); (0, 1) ] -> '-'
  | [ (-1, 0); (0, 1) ] -> 'L'
  | [ (-1, 0); (0, -1) ] -> 'J'
  | [ (1, 0); (0, -1) ] -> '7'
  | [ (1, 0); (0, 1) ] -> 'F'
  | _ -> failwith "invalid pipe"

let find_loop start resolve =
  let rec aux (row, col) path =
    if (row, col) = start && List.length path > 1
    then path
    else (
      let next =
        (match resolve (row, col) with
         | '|' -> [ (row + 1, col); (row - 1, col) ]
         | '-' -> [ (row, col + 1); (row, col - 1) ]
         | 'L' -> [ (row - 1, col); (row, col + 1) ]
         | 'J' -> [ (row - 1, col); (row, col - 1) ]
         | '7' -> [ (row + 1, col); (row, col - 1) ]
         | 'F' -> [ (row + 1, col); (row, col + 1) ]
         | _ -> [])
        |> List.filter (fun (row, col) ->
          0 <= row
          && row < rows
          && 0 <= col
          && col < cols
          && not (List.mem (row, col) path))
      in
      if next = [] then start :: path else aux (List.hd next) (List.hd next :: path))
  in
  aux start [ start ]

let () =
  (* read from argv *)
  let start = find_start lines in
  let pipe = infer_pipe start in
  Printf.printf "start: %s (%c)\n" (show_intpair start) pipe;
  let resolve (row, col) = if (row, col) = start then pipe else lines.(row).[col] in
  let path = find_loop start resolve in
  Printf.printf "Part 1: %d\n" (List.length path / 2);
  let tbl = Hashtbl.create 100 in
  List.iter (fun x -> Hashtbl.add tbl x ()) path;
  let is_on_path = Hashtbl.mem tbl in
  let is_horizontal = function
    | '-' | 'J' | 'L' -> true
    | _ -> false
  in
  let inside = ref 0 in
  for row = 0 to rows - 1 do
    let is_inside = ref false in
    for col = 0 to cols - 1 do
      let c = resolve (row, col) in
      if is_on_path (row, col) && not (is_horizontal c)
      then is_inside := not !is_inside
      else if not (is_on_path (row, col))
      then if !is_inside then inside := !inside + 1
    done
  done;
  Printf.printf "Part 2: %d\n" !inside
