open Base
open Stdio

module Point3D = struct
  type t = int * int * int [@@deriving compare, sexp, hash]

  let scorep2 (x1, _, _) (x2, _, _) = x1 * x2

  let distance (x1, y1, z1) (x2, y2, z2) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))
end

let boxes =
  In_channel.input_lines stdin
  |> List.map ~f:(fun line ->
    match String.split line ~on:',' |> List.map ~f:Int.of_string with
    | [ x; y; z ] -> (x, y, z)
    | _ -> failwith "invalid input")

let all_pairs lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      let new_pairs = List.map xs ~f:(fun y -> (x, y)) in
      aux (new_pairs @ acc) xs
  in
  aux [] lst

let distances =
  all_pairs boxes
  |> List.sort ~compare:(fun (b1, b2) (b3, b4) ->
    Int.compare (Point3D.distance b1 b2) (Point3D.distance b3 b4))

let enumerate_subgraphs graph =
  let visited = Hash_set.create (module Point3D) in
  let subgraphs = ref [] in
  Hashtbl.iter_keys graph ~f:(fun node ->
    if not (Hash_set.mem visited node)
    then (
      let current_subgraph = Hash_set.create (module Point3D) in
      let rec dfs n =
        if not (Hash_set.mem visited n)
        then (
          Hash_set.add visited n;
          Hash_set.add current_subgraph n;
          let neighbors = Hashtbl.find_multi graph n in
          List.iter neighbors ~f:dfs)
      in
      dfs node;
      subgraphs := current_subgraph :: !subgraphs));
  !subgraphs

let connect graph (p1, p2) =
  Hashtbl.add_multi graph ~key:p1 ~data:p2;
  Hashtbl.add_multi graph ~key:p2 ~data:p1

let part1 distances =
  let graph = Hashtbl.create (module Point3D) in
  List.take distances 1000 |> List.iter ~f:(connect graph);
  let circuits = enumerate_subgraphs graph in
  circuits
  |> List.map ~f:Hash_set.length
  |> List.sort ~compare:(fun a b -> -Int.compare a b)
  |> Fn.flip List.take 3
  |> List.fold ~init:1 ~f:( * )

let is_fully_connected graph nodes =
  List.for_all nodes ~f:(fun n -> Hashtbl.find_multi graph n |> Fn.non List.is_empty)

let part2 boxes distances =
  let graph = Hashtbl.create (module Point3D) in
  let p1, p2 =
    List.find_exn distances ~f:(fun (p1, p2) ->
      connect graph (p1, p2);
      is_fully_connected graph boxes)
  in
  Point3D.scorep2 p1 p2

let () =
  part1 distances |> printf "Part 1: %d\n";
  part2 boxes distances |> printf "Part 2: %d\n"
