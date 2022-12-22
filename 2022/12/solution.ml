open Graph

module G = Imperative.Digraph.Abstract (struct
  type t = int * int
end)

let g = G.create ()

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with
      | line -> Some line
      | exception End_of_file -> None)
  |> Array.of_seq

let rows = lines |> Array.length
let cols = lines.(0) |> String.length

let nodes =
  let new_node i j =
    let v = G.V.create (i, j) in
    G.add_vertex g v;
    v
  in
  Array.init rows (fun i -> Array.init cols (new_node i))

(* shortcuts for easier access *)
let node row col = nodes.(row).(col)
let char_at row col = lines.(row).[col]

let elevation row col =
  (match char_at row col with
  | 'S' -> Char.code 'a'
  | 'E' -> Char.code 'z'
  | c -> Char.code c)
  - Char.code 'a'

let traversable src dest = dest - src <= 1
let starting = ref (0, 0)
let ending = ref (0, 0)

(* Add the edges:
    two nodes are connected when the height difference is <= 1 *)
let () =
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let h = elevation row col in
      let n = node row col in
      if char_at row col = 'S' then starting := (row, col);
      if char_at row col = 'E' then ending := (row, col);
      (* Look up *)
      if row > 0 && traversable h (elevation (row - 1) col) then
        G.add_edge g n (node (row - 1) col);
      (* Look down *)
      if row < rows - 1 && traversable h (elevation (row + 1) col) then
        G.add_edge g n (node (row + 1) col);
      (* Look left *)
      if col > 0 && traversable h (elevation row (col - 1)) then
        G.add_edge g n (node row (col - 1));
      (* Look right *)
      if col < cols - 1 && traversable h (elevation row (col + 1)) then
        G.add_edge g n (node row (col + 1))
    done
  done

(* It's an unweighted graph *)
module W = struct
  type edge = G.E.t
  type t = int

  let weight _ = 1
  let zero = 0
  let add = ( + )
  let compare = compare
end

module P = Path.Dijkstra (G) (W)

let () =
  let start_i, start_j = !starting in
  let end_i, end_j = !ending in
  let start_node = node start_i start_j in
  let end_node = node end_i end_j in

  P.shortest_path g start_node end_node
  |> fst |> List.length
  |> Printf.printf "Part 1: %d\n";

  let nodes_to_check = ref [] in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if elevation i j = 0 then nodes_to_check := node i j :: !nodes_to_check
    done
  done;

  !nodes_to_check
  |> List.filter_map (fun n ->
         match P.shortest_path g n end_node with
         | path, _ -> Some (List.length path)
         | exception Not_found -> None)
  |> List.sort compare |> List.hd
  |> Printf.printf "Part 2: %d\n"
