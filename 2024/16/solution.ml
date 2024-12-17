open Base
open Stdio

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]

  let up = (-1, 0)
  let down = (1, 0)
  let left = (0, -1)
  let right = (0, 1)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let opp (x, y) = (-x, -y)
  let mul m (x, y) = (m * x, m * y)
  let ( = ) = equal

  let rotations dir =
    if dir = up || dir = down
    then [ left; right ]
    else if dir = left || dir = right
    then [ up; down ]
    else failwith "invalid direction"
end

module Line = struct
  type t = Point.t * Point.t [@@deriving sexp, compare, hash, equal]
end

let lines = In_channel.input_lines stdin |> List.to_array |> Array.map ~f:String.to_array
let xmax, ymax = (Array.length lines - 1, Array.length lines.(0) - 1)

let in_bounds (x, y) =
  0 <= x && x <= xmax && 0 <= y && y <= ymax && not (Char.equal lines.(x).(y) '#')

let rec find_char c x y =
  if x > xmax
  then failwith "Not found"
  else if y > ymax
  then find_char c (x + 1) 0
  else if Char.equal c lines.(x).(y)
  then (x, y)
  else find_char c x (y + 1)

let start = find_char 'S' 0 0
let end_pos = find_char 'E' 0 0

let neighbours (start, dir) =
  let rots =
    if Point.equal dir Point.up
    then [ (Point.up, 1); (Point.left, 1001); (Point.right, 1001) ]
    else if Point.equal dir Point.down
    then [ (Point.down, 1); (Point.left, 1001); (Point.right, 1001) ]
    else if Point.equal dir Point.left
    then [ (Point.left, 1); (Point.up, 1001); (Point.down, 1001) ]
    else if Point.equal dir Point.right
    then [ (Point.right, 1); (Point.up, 1001); (Point.down, 1001) ]
    else failwith "invalid direction"
  in
  List.filter_map rots ~f:(fun (dir, cost) ->
    let start = Point.add start dir in
    if in_bounds start then Some ((start, dir), cost) else None)

let dijkstra start =
  let dist = Hashtbl.Poly.create () in
  let prev = Hashtbl.Poly.create () in
  let pq = ref [ ((start, Point.right), 0) ] (* Priority queue (vertex, distance) *) in
  (* Initialize distances and prev map *)
  Hashtbl.set dist ~key:(start, Point.right) ~data:0;
  let rec process_queue () =
    match !pq with
    | [] -> ()
    | (u, d) :: pq_rest ->
      (* Remove the processed vertex from the priority queue *)
      pq := pq_rest;
      (* For each neighbor v of u, relax the edge (u, v) *)
      let relax (v, weight) =
        let new_dist = d + weight in
        match Hashtbl.find dist v with
        | None ->
          Hashtbl.set dist ~key:v ~data:new_dist;
          Hashtbl.set prev ~key:v ~data:[ u ];
          pq := insert_into_queue (v, new_dist) !pq
        | Some cost when new_dist < cost ->
          Hashtbl.set dist ~key:v ~data:new_dist;
          Hashtbl.set prev ~key:v ~data:[ u ];
          pq := insert_into_queue (v, new_dist) !pq
        | Some cost when new_dist = cost -> Hashtbl.add_multi prev ~key:v ~data:u
        | _ -> ()
      in
      neighbours u |> List.iter ~f:relax;
      process_queue ()
  (* Helper to insert into the priority queue, sorted by distance *)
  and insert_into_queue (v, d) pq =
    let rec insert = function
      | [] -> [ (v, d) ]
      | (x, dx) :: tl when dx > d -> (v, d) :: (x, dx) :: tl
      | hd :: tl -> hd :: insert tl
    in
    insert pq
  in
  (* Start processing the priority queue *)
  process_queue ();
  (* Return the distance and previous node mappings *)
  (dist, prev)

let () =
  let dist, prev = dijkstra start in
  let end_state =
    [ (end_pos, Point.up)
    ; (end_pos, Point.down)
    ; (end_pos, Point.left)
    ; (end_pos, Point.right)
    ]
    |> List.filter ~f:(Hashtbl.mem dist)
    |> List.hd_exn
  in
  let p1 = Hashtbl.find_exn dist end_state in
  printf "Part 1: %d\n" p1;
  let points = Hash_set.create (module Point) in
  let rec find_path (p, dir) =
    Hash_set.add points p;
    Hashtbl.find_multi prev (p, dir) |> List.iter ~f:find_path
  in
  find_path end_state;
  printf "Part 2: %d\n" (Hash_set.length points)
