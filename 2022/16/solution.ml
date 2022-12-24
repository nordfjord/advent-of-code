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

let valve_by_name name = Hashtbl.find tbl name

let rec part1_solution max unvisited path pressure time (node, distance) =
  let next_time = time - (distance + 1) in
  let next_pressure = pressure + (node.flow_rate * next_time) in
  let next_path = node.name :: path in
  let next_nodes =
    unvisited
    |> List.filter_map (fun name ->
           if not (List.mem name next_path) then
             let shortest_path =
               Hashtbl.find shortest_paths (node.name, name)
             in
             Some (valve_by_name name, shortest_path)
           else None)
  in

  if next_time < 0 then (if !max < pressure then max := pressure)
  else if List.length next_nodes = 0 then (
    if !max < next_pressure then max := next_pressure)
  else
    List.iter
      (fun (node, dist) ->
        let next_unvisited =
          unvisited |> List.filter (fun v -> v <> node.name)
        in
        part1_solution max next_unvisited next_path next_pressure next_time
          (node, dist))
      next_nodes

let openable_valves =
  valves |> List.filter (fun v -> v.flow_rate > 0) |> List.map (fun v -> v.name)

let part1 () =
  printf "Starting part 1\n%!";
  let starting_node = Hashtbl.find tbl "AA" in
  let unvisited = openable_valves in
  let max = ref 0 in
  let () = part1_solution max unvisited [] 0 30 (starting_node, -1) in

  printf "Part 1: %d\n%!" !max

module StrSet = Set.Make (String)

let rec combinations k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h = List.map (fun l -> h :: l) (combinations (k - 1) tl) in
        let without_h = combinations k tl in
        with_h @ without_h

module T = Domainslib.Task

let rec update_max max new_value =
  let current = Atomic.get max in
  if new_value > current then
    if Atomic.compare_and_set max current new_value then
      printf "Maximum=%d\n" new_value
    else (
      Domain.cpu_relax ();
      update_max max new_value)

let part2_solution () =
  let starting_node = Hashtbl.find tbl "AA" in
  let valve_count = openable_valves |> List.length in
  let maximum_pressure = Atomic.make 0 in
  printf "%d Valves to check\n" valve_count;
  let valve_set = StrSet.of_list openable_valves in
  let pool = T.setup_pool ~num_domains:12 () in
  T.run pool (fun _ ->
      Seq.ints 0
      |> Seq.take (valve_count / 2)  (* The assumption is that we only need to check half the paths because it'll be mirrored if we go beyond half *)
      |> List.of_seq
      |> List.map (fun i ->
             T.async pool (fun _ ->
                 printf "Starting computation for %d\n" i;
                 combinations i openable_valves
                 |> List.map (fun unvisited ->
                        T.async pool (fun _ ->
                            let others =
                              StrSet.of_list unvisited |> StrSet.diff valve_set
                              |> StrSet.to_seq |> List.of_seq
                            in
                            let find_best unvisited =
                              T.async pool (fun _ ->
                                  let best = ref 0 in
                                  part1_solution best unvisited [] 0 26
                                    (starting_node, -1);
                                  !best)
                            in
                            let best1 = find_best unvisited in
                            let best2 = find_best others in
                            let pressure =
                              T.await pool best1 + T.await pool best2
                            in
                            update_max maximum_pressure pressure))
                 |> List.iter (T.await pool)))
      |> List.iter (T.await pool));
  T.teardown_pool pool;
  Atomic.get maximum_pressure

let part2 () =
  printf "Starting part 2\n";
  let max = part2_solution () in
  printf "Part 2: %d\n" max

let () =
  part1 ();
  part2 ()
