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
  |> Array.of_list

let box_id b =
  let rec aux i =
    if i >= Array.length boxes
    then failwith "box not found"
    else if Point3D.compare boxes.(i) b = 0
    then i
    else aux (i + 1)
  in
  aux 0

let all_pairs lst =
  Sequence.range 0 (Array.length lst - 1)
  |> Sequence.concat_map ~f:(fun i ->
    Sequence.range (i + 1) (Array.length lst - 1)
    |> Sequence.map ~f:(fun j -> (lst.(i), lst.(j))))
  |> Sequence.to_list

let distances =
  all_pairs boxes
  |> List.sort ~compare:(fun (b1, b2) (b3, b4) ->
    Int.compare (Point3D.distance b1 b2) (Point3D.distance b3 b4))
  |> List.map ~f:(fun (p1, p2) -> (box_id p1, box_id p2))

module DSU = struct
  type t =
    { parents : int array
    ; size : int array
    }

  let make_set n = { parents = Array.init n ~f:Fn.id; size = Array.create ~len:n 1 }

  (* A common gotcha is forgetting to check if elements are already in the same set
   before attempting a union, leading to unnecessary operations. *)

  let find ds element =
    let rec aux current =
      let root = ds.parents.(current) in
      if root <> current
      then (
        let root = aux root in
        ds.parents.(current) <- root;
        root)
      else current
    in
    aux element

  let size ds element =
    let rep = find ds element in
    ds.size.(rep)

  let roots ds =
    Array.foldi ds.parents ~init:[] ~f:(fun i acc parent ->
      if i = parent then i :: acc else acc)

  let union ds elem1 elem2 =
    let irep = find ds elem1 in
    let jrep = find ds elem2 in
    if irep <> jrep
    then (
      let isize = ds.size.(irep) in
      let jsize = ds.size.(jrep) in
      if isize < jsize
      then (
        ds.parents.(irep) <- jrep;
        ds.size.(jrep) <- ds.size.(jrep) + isize)
      else (
        ds.parents.(jrep) <- irep;
        ds.size.(irep) <- ds.size.(irep) + jsize))
end

let part1 dists =
  let dsu = DSU.make_set 1000 in
  Sequence.of_list dists
  |> Fn.flip Sequence.take 1000
  |> Sequence.iter ~f:(fun (i1, i2) -> DSU.union dsu i1 i2);
  let sizes = DSU.roots dsu |> List.map ~f:(DSU.size dsu) |> Array.of_list in
  Array.sort sizes ~compare:(fun a b -> -Int.compare a b);
  sizes.(0) * sizes.(1) * sizes.(2)

let part2 distances =
  let dsu = DSU.make_set 1000 in
  let p1, p2 =
    List.find_exn distances ~f:(fun (i1, i2) ->
      DSU.union dsu i1 i2;
      DSU.size dsu i1 = 999)
  in
  Point3D.scorep2 boxes.(p1) boxes.(p2)

let () =
  part1 distances |> printf "Part 1: %d\n";
  part2 distances |> printf "Part 2: %d\n"
