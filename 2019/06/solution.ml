open Printf
open Prelude

let input =
  Aoc.stdin_seq ()
  |> Seq.map (fun s ->
       match String.split_on_char ')' s with
       | src :: dest :: _ -> (src, dest)
       | _ -> failwith "Invalid input")
  |> Array.of_seq

let rec count_orbits nodes orbits =
  List.length orbits
  + (orbits
    |> List.map (Hashtbl.find nodes)
    |> List.map (count_orbits nodes)
    |> List.fold_left ( + ) 0)

let () =
  let nodes = Hashtbl.create 1000 in
  input
  |> Array.iter (fun (dst, src) ->
       if not (Hashtbl.mem nodes dst) then Hashtbl.add nodes dst [];
       let orbits =
         match Hashtbl.find_opt nodes src with
         | Some node -> node
         | None -> []
       in
       Hashtbl.replace nodes src (dst :: orbits));
  nodes
  |> Hashtbl.to_seq_values
  |> Seq.map (count_orbits nodes)
  |> Seq.fold_left ( + ) 0
  |> printf "Part 1: %d\n"

let bfs nodes start dst =
  let visited = Hashtbl.create (Hashtbl.length nodes) in
  Hashtbl.add visited start ();
  let q = Queue.create () in
  Queue.add (0, start) q;
  let result = ref 0 in
  while not (Queue.is_empty q) do
    let distance, node = Queue.take q in
    if node = dst
    then (
      result := distance;
      Queue.clear q)
    else (
      let orbits = Hashtbl.find nodes node in
      orbits
      |> List.filter (fun n -> not (Hashtbl.mem visited n))
      |> List.iter (fun next ->
           Hashtbl.add visited next ();
           Queue.add (distance + 1, next) q))
  done;
  !result

let () =
  let nodes = Hashtbl.create 1000 in
  input
  |> Array.iter (fun (dst, src) ->
       let dst_orbits =
         match Hashtbl.find_opt nodes dst with
         | Some node -> node
         | None -> []
       in
       let src_orbits =
         match Hashtbl.find_opt nodes src with
         | Some node -> node
         | None -> []
       in
       Hashtbl.replace nodes src (dst :: src_orbits);
       Hashtbl.replace nodes dst (src :: dst_orbits));
  bfs nodes "YOU" "SAN" - 2 |> printf "Part 2: %d\n"
