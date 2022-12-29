open Printf

let t = Sys.time ()

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with x -> Some x | exception End_of_file -> None)

type valve = { name : string; flow_rate : int; paths : string list }

let valves =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line "Valve %s has flow rate=%d; %_s %_s to %_s %s@$"
           (fun name flow_rate valves ->
             let paths = valves |> Str.split (Str.regexp_string ", ") in
             { name; flow_rate; paths }))
  |> Array.of_seq

let shortest_paths =
  let len = Array.length valves in
  let dist =
    Array.init len (fun i ->
        Array.init len (fun j ->
            if i = j then 0
            else
              let node = valves.(i) in
              let name = valves.(j).name in
              if List.mem name node.paths then 1 else 100000))
  in
  for k = 0 to len - 1 do
    for i = 0 to len - 1 do
      for j = 0 to len - 1 do
        if dist.(i).(j) > dist.(i).(k) + dist.(k).(j) then
          dist.(i).(j) <- dist.(i).(k) + dist.(k).(j)
      done
    done
  done;
  dist

let valve_by_name i = valves.(i)

module IntSet = Set.Make (Int)

let rec dfs max_pressure unvisited pressure time (i, distance) =
  let next_time = time - (distance + 1) in
  let node = valve_by_name i in
  let next_pressure = pressure + (node.flow_rate * next_time) in
  let next_nodes =
    unvisited |> IntSet.to_seq
    |> Seq.map (fun j -> (j, shortest_paths.(i).(j)))
    |> List.of_seq
  in

  if next_time < 0 then max_pressure := max !max_pressure pressure
  else if List.length next_nodes = 0 then
    max_pressure := max !max_pressure next_pressure
  else
    List.iter
      (fun (node, dist) ->
        let next_unvisited = unvisited |> IntSet.remove node in
        dfs max_pressure next_unvisited next_pressure next_time (node, dist))
      next_nodes

let openable_valves =
  valves |> Array.to_seqi
  |> Seq.filter_map (fun (i, v) -> if v.flow_rate > 0 then Some i else None)
  |> List.of_seq

let starting_node =
  valves |> Array.to_seqi
  |> Seq.find_map (fun (i, v) -> if v.name = "AA" then Some i else None)
  |> Option.get

let () =
  let unvisited = IntSet.of_list openable_valves in
  let max = ref 0 in
  dfs max unvisited 0 30 (starting_node, -1);

  printf "Part 1: %d\n" !max

let rec combinations k list =
  if k <= 0 then Seq.return IntSet.empty
  else
    match list with
    | [] -> Seq.return IntSet.empty
    | h :: tl ->
        let with_h = Seq.map (IntSet.add h) (combinations (k - 1) tl) in
        let without_h = combinations k tl in
        Seq.append with_h without_h

let () =
  let valve_count = openable_valves |> List.length in
  let best = Hashtbl.create 10000 in
  let find_best unvisited =
    if Hashtbl.mem best unvisited then ()
    else
      let pressure = ref 0 in
      dfs pressure unvisited 0 26 (starting_node, -1);
      Hashtbl.add best unvisited !pressure
  in

  (* The assumption is that we only need to check half the paths because it'll be mirrored if we go beyond half *)
  combinations ((valve_count / 2) - 1) openable_valves |> Seq.iter find_best;
  printf "Found %d solutions\n" (Hashtbl.length best);

  let arr = Array.of_seq (Hashtbl.to_seq best) in

  arr
  |> Array.fold_left
       (fun max_pressure (left_valves, left_pressure) ->
         arr
         |> Array.fold_left
              (fun max_pressure (right_valves, right_pressure) ->
                let pressure = left_pressure + right_pressure in
                if
                  pressure > max_pressure
                  && IntSet.disjoint left_valves right_valves
                then pressure
                else max_pressure)
              max_pressure)
       0
  |> printf "Part 2: %d\n";

  printf "\nRunning time %f\n" (Sys.time () -. t)
