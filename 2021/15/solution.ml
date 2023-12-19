open Base
open Stdio

type grid = int array array [@@deriving sexp, compare, show]

let grid =
  In_channel.input_lines stdin
  |> List.map ~f:(fun s ->
    String.to_array s |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))
  |> Array.of_list

let adjacent (row, col) =
  [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]

let lowest_cost_item tbl =
  match
    Hashtbl.fold tbl ~init:None ~f:(fun ~key ~data acc ->
      match acc with
      | None -> Some (key, data)
      | Some (_, cost) when data < cost -> Some (key, data)
      | Some _ -> acc)
  with
  | Some (key, data) ->
    Hashtbl.remove tbl key;
    (key, data)
  | None -> failwith "empty"

let dijkstra resolve start goal =
  let q = Hashtbl.Poly.create () in
  let distances = Hashtbl.Poly.create () in
  let result = ref Int.max_value in
  Hashtbl.set q ~key:start ~data:0;
  Hashtbl.add_exn distances ~key:start ~data:0;
  while not (Hashtbl.is_empty q) do
    let (row, col), cost = lowest_cost_item q in
    if Poly.((row, col) = goal)
    then (
      result := cost;
      Hashtbl.clear q)
    else
      adjacent (row, col)
      |> List.filter_map ~f:(fun (row, col) -> resolve (row, col))
      |> List.iter ~f:(fun (row', col', cost') ->
        let cost = cost + cost' in
        let should_explore =
          match Hashtbl.find distances (row', col') with
          | None -> true
          | Some c -> cost < c
        in
        if should_explore
        then (
          Hashtbl.set distances ~key:(row', col') ~data:cost;
          Hashtbl.set q ~key:(row', col') ~data:cost))
  done;
  !result

let divrem a b =
  let q = a / b in
  let r = a % b in
  (q, r)

let () =
  let resolve (x, y) =
    if x < 0 || y < 0 || x >= Array.length grid || y >= Array.length grid.(0)
    then None
    else Some (x, y, grid.(x).(y))
  in
  dijkstra resolve (0, 0) (Array.length grid - 1, Array.length grid - 1) |> printf "%d\n";
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let buckets = 5 in
  let resolve (x, y) =
    let x_bucket, x_offset = divrem x rows in
    let y_bucket, y_offset = divrem y cols in
    if x < 0 || x_bucket >= buckets || y < 0 || y_bucket >= buckets
    then None
    else (
      let c = grid.(x_offset).(y_offset) in
      let cost =
        match (c + y_bucket + x_bucket) % 9 with
        | 0 -> 9
        | c -> c
      in
      Some (x, y, cost))
  in
  dijkstra resolve (0, 0) ((rows * buckets) - 1, (cols * buckets) - 1) |> printf "%d\n"
