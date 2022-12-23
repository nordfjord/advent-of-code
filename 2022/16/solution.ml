open Printf
open Graph

module G = Imperative.Graph.Abstract (struct
  type t = string
end)

let g = G.create ()

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with x -> Some x | exception End_of_file -> None)

type valve = { name : string; flow_rate : int; paths : string list }

let valves =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line "Valve %s has flow rate=%d; %_s %_s to %_s %s@\n"
           (fun name flow_rate valves ->
             let paths = valves |> Str.split (Str.regexp_string ", ") in
             { name; flow_rate; paths }))
  |> List.of_seq

let tbl = Hashtbl.create 100

let () =
  valves
  |> List.iter (fun valve ->
         let v = G.V.create valve.name in
         G.add_vertex g v;
         G.Mark.set v valve.flow_rate;
         Hashtbl.add tbl valve.name v)

let () =
  valves
  |> List.iter (fun valve ->
         valve.paths
         |> List.iter (fun name ->
                let src_vertex = Hashtbl.find tbl valve.name in
                let dst_vertex = Hashtbl.find tbl name in
                G.add_edge g src_vertex dst_vertex))

let valve_by_name name = Hashtbl.find tbl name

module StringSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let openable_valve_count =
  valves |> List.filter (fun v -> v.flow_rate = 0) |> List.length

let minutes_left = ref 30


module W = struct
  type edge = G.E.t
  type t = int

  let weight _ = 1
  let compare = compare
  let add = ( + )
  let sub = ( - )
  let zero = 0

end

module Bfs = Traverse.Bfs(G)
module Dfs = Traverse.Dfs(G)
module Dijkstra = Path.Dijkstra(G)(W)

let highest_value_path g =
  Bfs.iter (fun v -> 
    let flow_rate = G.Mark.get v in
    let name = G.V.label v in
    printf "Visited valve %s. Flow rate=%d" name flow_rate

)


let part1 () =
  let initial_open = StringSet.empty in
  let starting_node = Hashtbl.find tbl "AA" in
  highest_value_path g

let () = part1 ()
