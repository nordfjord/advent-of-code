open Printf
open Graph

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with x -> Some x | exception End_of_file -> None)

type valve = { name : string; flow_rate : int; paths : string list }

module G = Imperative.Graph.Abstract (struct
  type t = string
end)

let g = G.create ()
let () = printf "Starting...\n%!"

let valves =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line "Valve %s has flow rate=%d; %_s %_s to %_s %s@$"
           (fun name flow_rate valves ->
             let paths = valves |> Str.split (Str.regexp_string ", ") in
             { name; flow_rate; paths }))
  |> List.of_seq

let () = printf "Parsed valves\n%!"
let tbl = Hashtbl.create 100
let () = valves |> List.iter (fun valve -> Hashtbl.add tbl valve.name valve)
let vertices = Hashtbl.create 100

let () =
  valves
  |> List.iter (fun valve ->
         let v = G.V.create valve.name in
         G.add_vertex g v;
         Hashtbl.add vertices valve.name v)

let () =
  valves
  |> List.iter (fun valve ->
         let src = Hashtbl.find vertices valve.name in
         valve.paths
         |> List.iter (fun name ->
                let dst = Hashtbl.find vertices name in
                G.add_edge g src dst))

module W = struct
  type edge = G.E.t
  type t = int

  let weight _ = 1
  let zero = 0
  let add = ( + )
  let compare = compare
end

module Dijkstra = Path.Dijkstra (G) (W)

let shortest_paths =
  printf "Creating shortest path matrix\n%!";
  let tbl = Hashtbl.create 1000 in
  g
  |> G.iter_vertex (fun src ->
         g
         |> G.iter_vertex (fun dst ->
                let src_name = G.V.label src in
                let dst_name = G.V.label dst in
                if src <> dst && not (Hashtbl.mem tbl (src_name, dst_name)) then
                  let path, _ = Dijkstra.shortest_path g src dst in
                  Hashtbl.add tbl (src_name, dst_name) (List.length path)));

  printf "Shortest paths prepared\n%!";
  tbl

let rec part1_solution max opened pressure time (node, distance) =
  let next_time = time - (distance + 1) in
  let next_pressure = pressure + (node.flow_rate * next_time) in
  let next_opened = node.name :: opened in
  let next_nodes =
    valves
    |> List.filter_map (fun v ->
           if
             v.name <> node.name && v.flow_rate > 0
             && not (List.mem v.name opened)
           then
             let shortest_path =
               Hashtbl.find shortest_paths (node.name, v.name)
             in
             Some (v, shortest_path)
           else None)
  in

  if next_time < 0 then (if !max < pressure then max := pressure)
  else if List.length next_nodes = 0 then (
    if !max < next_pressure then max := next_pressure)
  else
    List.iter
      (part1_solution max next_opened next_pressure next_time)
      next_nodes

let rec pairs list =
  match list with
  | [] -> []
  | head :: tail ->
      ((head, head) :: List.map (fun x -> (head, x)) tail) @ pairs tail

let add_to_open v opened = if List.mem v opened then opened else v :: opened

module T = Domainslib.Task

module LockfreeMaxInt = struct
  let rec update atom v =
    let current = Atomic.get atom in
    if current < v then
      if Atomic.compare_and_set atom current v then ()
      else (
        Domain.cpu_relax ();
        update atom v)
end

let rec part2_solution pool max opened pressure time (distance1, distance2)
    (node1, node2) =
  if time = 0 || (distance1 < 0 && distance2 < 0) then (
    let m = Atomic.get max in
    if m < pressure then (
      LockfreeMaxInt.update max pressure;
      printf "Finshed. max=%d; Opened=%s\n%!" pressure
        (opened |> String.concat ",")))
  else
    let possible_next_nodes =
      valves
      |> List.filter (fun v ->
             v.flow_rate > 0 && v.name <> node1.name && v.name <> node2.name
             && not (List.mem v.name opened))
    in

    let opened = ref opened in
    let next_distances = ref [] in
    let next_nodes = ref [] in
    let next_pressure = ref pressure in
    if distance1 = 0 || distance2 = 0 then (
      if distance1 = 0 && not (List.mem node1.name !opened) then (
        next_pressure := !next_pressure + (node1.flow_rate * time);
        next_nodes := possible_next_nodes |> List.map (fun n -> (n, node2));
        next_distances :=
          possible_next_nodes
          |> List.map (fun n ->
                 ( Hashtbl.find shortest_paths (node1.name, n.name),
                   distance2 - 1 ));
        opened := add_to_open node1.name !opened);
      if distance2 = 0 && not (List.mem node2.name !opened) then (
        next_pressure := !next_pressure + (node2.flow_rate * time);
        next_nodes := possible_next_nodes |> List.map (fun n -> (node1, n));
        next_distances :=
          possible_next_nodes
          |> List.map (fun n ->
                 ( distance1 - 1,
                   Hashtbl.find shortest_paths (node2.name, n.name) ));
        opened := add_to_open node2.name !opened);
      if distance1 = 0 && distance2 = 0 && List.length possible_next_nodes > 0
      then (
        next_nodes := possible_next_nodes |> pairs;
        next_distances :=
          !next_nodes
          |> List.map (fun (new1, new2) ->
                 ( Hashtbl.find shortest_paths (node1.name, new1.name),
                   Hashtbl.find shortest_paths (node2.name, new2.name) ))));

    let time = time - 1 in
    if List.length !next_nodes = 0 then
      part2_solution pool max !opened !next_pressure time
        (distance1 - 1, distance2 - 1)
        (node1, node2)
    else
      let next_nodes = !next_nodes |> List.to_seq in
      let distances = !next_distances |> List.to_seq in
      Seq.zip next_nodes distances
      |> Seq.map (fun (node, distance) ->
             T.async pool (fun _ ->
                 part2_solution pool max !opened !next_pressure time distance
                   node))
      |> Seq.iter (T.await pool)

let part1 () =
  printf "Starting part 1\n";
  let starting_node = Hashtbl.find tbl "AA" in
  let max = ref 0 in
  let () = part1_solution max [] 0 30 (starting_node, -1) in

  printf "Part 1: %d\n" !max

let part2 () =
  printf "Starting part 2\n";
  let starting_node = Hashtbl.find tbl "AA" in
  let max = Atomic.make 0 in
  let pool = T.setup_pool ~num_domains:14 () in
  T.run pool (fun _ ->
      part2_solution pool max [] 0 26 (0, 0) (starting_node, starting_node));
  T.teardown_pool pool;
  printf "Part 2: %d\n" (Atomic.get max)

let () = 
  part1 ();
  part2 ()
