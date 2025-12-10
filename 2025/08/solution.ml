open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

module Point3D = struct
  type t = int * int * int [@@deriving compare, sexp, hash, show, equal]

  let scorep2 (x1, _, _) (x2, _, _) = x1 * x2

  let distance (x1, y1, z1) (x2, y2, z2) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))

  let distance_compare (p1, p2) (p3, p4) = Int.compare (distance p1 p2) (distance p3 p4)
end

let boxes, lookup =
  let arr = Array.create ~len:1000 (0, 0, 0) in
  let tbl = Hashtbl.create (module Point3D) in
  let rec aux i =
    if i >= 1000
    then (arr, tbl)
    else (
      let line = In_channel.input_line_exn stdin in
      arr.(i) <- Stdlib.Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z));
      Hashtbl.add_exn tbl ~key:arr.(i) ~data:i;
      aux (i + 1))
  in
  aux 0

let box_id = Hashtbl.find_exn lookup

module Heap = Prelude.Heap.Make (struct
    type t = Point3D.t * Point3D.t

    let compare = Point3D.distance_compare
  end)

let ordered_pairs arr =
  let result =
    Heap.create
      ~dummy:((0, 0, 0), (0, 0, 0))
      ((Array.length arr - 1) * (Array.length arr / 2))
  in
  for i = 0 to Array.length arr - 1 do
    for j = i + 1 to Array.length arr - 1 do
      Heap.add result (arr.(i), arr.(j))
    done
  done;
  result

let edges = ordered_pairs boxes

module DSU = struct
  type t =
    { parents : int array
    ; size : int array
    }

  let create n = { parents = Array.init n ~f:Fn.id; size = Array.create ~len:n 1 }

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

  let size ds element = ds.size.(find ds element)
  let roots ds = Array.filteri ds.parents ~f:( = )

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
        ds.size.(jrep) <- isize + jsize)
      else (
        ds.parents.(jrep) <- irep;
        ds.size.(irep) <- isize + jsize))
end

let part1 edges () =
  let dsu = DSU.create (Array.length boxes) in
  let rec aux i =
    if i < 1000
    then (
      let i1, i2 = Heap.pop_minimum edges in
      DSU.union dsu (box_id i1) (box_id i2);
      aux (i + 1))
  in
  aux 0;
  let sizes = DSU.roots dsu |> Array.map ~f:(DSU.size dsu) in
  Array.sort sizes ~compare:(Comparable.compare_reversed Int.compare);
  sizes.(0) * sizes.(1) * sizes.(2)

let part2 edges () =
  let dsu = DSU.create (Array.length boxes) in
  let rec aux () =
    let i1, i2 = Heap.pop_minimum edges in
    DSU.union dsu (box_id i1) (box_id i2);
    if DSU.size dsu (box_id i1) = Array.length boxes
    then Point3D.scorep2 i1 i2
    else aux ()
  in
  aux ()

let () =
  Prelude.Runner.run (part1 (Heap.copy edges)) (part2 edges);
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
