open Base
open Stdio

type point_3d = int * int * int [@@deriving show]
type moon = point_3d * point_3d [@@deriving show]

let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let positions =
  In_channel.input_lines stdin
  |> List.map ~f:(fun s ->
    Stdlib.Scanf.sscanf s "<x=%d, y=%d, z=%d>" (fun x y z -> (x, y, z)))
  |> List.map ~f:(fun p -> (p, (0, 0, 0)))

let gravity a b = if b > a then 1 else if b < a then -1 else 0
let gravity_3d (x1, y1, z1) (x2, y2, z2) = (gravity x1 x2, gravity y1 y2, gravity z1 z2)

let simulate positions =
  positions
  |> List.map ~f:(fun p ->
    let pos, vel = p in
    let new_vel =
      positions
      |> List.filter ~f:(Poly.( <> ) p)
      |> List.map ~f:fst
      |> List.fold ~init:vel ~f:(fun vel other -> add vel (gravity_3d pos other))
    in
    (pos, new_vel))
  |> List.map ~f:(fun (p, v) -> (add p v, v))

let score l =
  List.sum
    (module Int)
    l
    ~f:(fun ((x, y, z), (dx, dy, dz)) ->
      let pot = abs x + abs y + abs z in
      let kin = abs dx + abs dy + abs dz in
      pot * kin)

let part1 () =
  Sequence.range 0 1000
  |> Sequence.fold ~init:positions ~f:(fun pos _ -> simulate pos)
  |> score
  |> printf "Part 1: %d\n"

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let lcm a b =
  match (a, b) with
  | 0, _ | _, 0 -> 0
  | a, b -> abs (a * b) / gcd a b

let find_cycle f initial =
  let comp = List.map initial ~f in
  let rec find state count =
    if List.equal [%equal: int * int] (List.map state ~f) comp
    then count
    else find (simulate state) (count + 1)
  in
  find (simulate initial) 1

let get_x ((x, _, _), (x2, _, _)) = (x, x2)
let get_y ((_, y, _), (_, y2, _)) = (y, y2)
let get_z ((_, _, z), (_, _, z2)) = (z, z2)

let part2 () =
  let x = find_cycle get_x positions in
  let y = find_cycle get_y positions in
  let z = find_cycle get_z positions in
  printf "x=%d; y=%d; z=%d\n" x y z;
  printf "Part 2: %d\n" (x |> lcm y |> lcm z)

let () =
  part1 ();
  part2 ()
