open Printf

let lines =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None )
  |> Array.of_seq

type point = int * int

module PointSet = Set.Make (struct
  type t = point

  let compare = compare
end)

let winds =
  lines |> Array.to_seq
  |> Seq.mapi (fun row line ->
         line |> String.to_seqi
         |> Seq.filter_map (fun (col, c) ->
                match c with
                | '>' ->
                    Some ((row, col), (0, 1))
                | '<' ->
                    Some ((row, col), (0, -1))
                | '^' ->
                    Some ((row, col), (-1, 0))
                | 'v' ->
                    Some ((row, col), (1, 0))
                | _ ->
                    None ) )
  |> Seq.concat
  |> Seq.map (fun ((row, col), vec) -> ((row - 1, col - 1), vec))
  |> List.of_seq

let rows = Array.length lines - 2

let cols = String.length lines.(0) - 2

let ( % ) n m = ((n mod m) + m) mod m

let advance_wind ((row, col), (delta_rows, delta_cols)) =
  ( ((row + delta_rows) % rows, (col + delta_cols) % cols)
  , (delta_rows, delta_cols) )

let wind_states =
  Seq.unfold
    (fun prev_winds ->
      let next = List.map advance_wind prev_winds in
      if next = winds then None else Some (next, next) )
    winds
  |> Seq.append (Seq.return winds)
  |> Seq.map (List.map fst)
  |> Seq.map PointSet.of_list |> Array.of_seq

let wind_cycle = Array.length wind_states

let adjacent_and_self (row, col) =
  [(row + 1, col); (row - 1, col); (row, col + 1); (row, col - 1); (row, col)]

let is_in_bounds (row, col) =
  (row, col) = (rows, cols - 1)
  || (row, col) = (-1, 0)
  || (0 <= row && row < rows && 0 <= col && col < cols)

let possible_moves (row, col) winds =
  adjacent_and_self (row, col)
  |> List.filter (fun p -> is_in_bounds p && not (PointSet.mem p winds))

let bfs pos goal initial_minute =
  let next_winds m = wind_states.((m + 1 + initial_minute) mod wind_cycle) in
  let q = Queue.create () in
  Queue.push (0, pos) q ;
  let result = ref 0 in
  let seen_states = Hashtbl.create 100000 in
  while (not (Queue.is_empty q)) && !result = 0 do
    let mins, pos = Queue.take q in
    if pos = goal then (
      result := mins ;
      Queue.clear q )
    else if Hashtbl.mem seen_states (mins mod wind_cycle, pos) then ()
    else (
      Hashtbl.add seen_states (mins mod wind_cycle, pos) () ;
      let next_positions = possible_moves pos (next_winds mins) in
      next_positions |> List.iter (fun p -> Queue.add (mins + 1, p) q) )
  done ;
  !result

let () =
  let goal = (rows, cols - 1) in
  let start = (-1, 0) in
  let result = bfs start goal 0 in
  printf "Part 1: %d\n" result ;
  let snack_trip = bfs goal start result in
  let trip_back = bfs start goal (result + snack_trip) in
  printf "Part 2: %d\n" (result + snack_trip + trip_back)
