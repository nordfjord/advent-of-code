open Format
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

let node i j = nodes.(i).(j) (* shortcut for easier access *)

let char_at i j = lines.(i).[j]

let elevation i j =
  match char_at i j with
  | 'S' ->  Char.code 'a'
  | 'E' -> Char.code 'z' 
  | c -> Char.code c

let traversable src dest = dest - src <= 1
let starting = ref (0, 0)
let ending = ref (0, 0)

(* We add the edges:
   two nodes are connected whenever they can't have the same value,
   i.e. they belong to the same line, the same column or the same 3x3 group *)
let () =
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let h = elevation i j in
      let n = node i j in
      G.Mark.set n h;
      if char_at i j = 'S' then (
        Printf.printf "Starting point found\n";
        starting := (i, j));
      if char_at i j = 'E' then (
        Printf.printf "Ending point found\n";
        ending := (i, j));
      (* Look up *)
      if i > 0 && traversable h (elevation (i - 1) j) then
        G.add_edge g n (node (i - 1) j);
      (* Look down *)
      if i < rows - 1 && traversable h (elevation (i + 1) j) then
        G.add_edge g n (node (i + 1) j);
      (* Look left *)
      if j > 0 && traversable h (elevation i (j - 1)) then
        G.add_edge g n (node i (j - 1));
      (* Look right *)
      if j < cols - 1 && traversable h (elevation i (j + 1)) then
        G.add_edge g n (node i (j + 1))
    done
  done

module W = struct
  type edge = G.E.t
  type t = int

  let weight _ = 1
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = compare
end

module P = Path.Dijkstra (G) (W)

let () =
  let start_i, start_j = !starting in
  let end_i, end_j = !ending in
  let start_node = node start_i start_j in
  let end_node = node end_i end_j in
  Printf.printf "\n\n%d,%d -> %d,%d\n" start_i start_j end_i end_j;
  let path, _weight = P.shortest_path g start_node end_node in
  Printf.printf "Part 1: %d\n" (List.length path)
