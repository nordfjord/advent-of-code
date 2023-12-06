open Printf

let lines =
  Seq.of_dispenser (fun () ->
    match read_line () with
    | line -> Some line
    | exception End_of_file -> None)
  |> Array.of_seq

let rows = lines |> Array.length
let cols = lines.(0) |> String.length

(* shortcuts for easier access *)
let char_at (row, col) = lines.(row).[col]

let elevation pos =
  (match char_at pos with
   | 'S' -> Char.code 'a'
   | 'E' -> Char.code 'z'
   | c -> Char.code c)
  - Char.code 'a'

let traversable src dest = dest - src <= 1

let starting, ending =
  lines
  |> Array.to_seqi
  |> Seq.fold_left
       (fun (starting, ending) (row, cols) ->
         cols
         |> String.to_seqi
         |> Seq.fold_left
              (fun (starting, ending) (col, c) ->
                if c = 'S'
                then ((row, col), ending)
                else if c = 'E'
                then (starting, (row, col))
                else (starting, ending))
              (starting, ending))
       ((0, 0), (0, 0))

let possible_moves (row, col) =
  [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]
  |> List.filter (fun (r, c) ->
    r >= 0
    && r < rows
    && c >= 0
    && c < cols
    && traversable (elevation (row, col)) (elevation (r, c)))

let bfs node goal =
  let q = Queue.create () in
  Queue.add (0, node) q;
  let result = ref Int.max_int in
  let visited = Hashtbl.create 3000 in
  while not (Queue.is_empty q) do
    let dist, pos = Queue.take q in
    if Hashtbl.mem visited pos
    then ()
    else if pos = goal
    then (
      result := dist;
      Queue.clear q)
    else (
      Hashtbl.add visited pos ();
      possible_moves pos |> List.iter (fun p -> Queue.add (dist + 1, p) q))
  done;
  !result

let () =
  bfs starting ending |> printf "Part 1: %d\n";
  Array.to_seqi lines
  |> Seq.concat_map (fun (row, s) ->
    s
    |> String.to_seqi
    |> Seq.filter_map (fun (col, c) ->
      if c = 'a' || c = 'S' then Some (row, col) else None))
  |> Seq.map (fun start -> bfs start ending)
  |> Seq.fold_left min Int.max_int
  |> printf "Part 2: %d\n"
