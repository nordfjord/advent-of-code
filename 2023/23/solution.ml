open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.map ~f:String.to_array |> List.to_array
let start = (0, 1)
let goal = (Array.length lines - 1, Array.length lines.(0) - 2)

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]
end

let adjacent grid (x, y) =
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter ~f:(fun (x, y) ->
    x >= 0
    && y >= 0
    && x < Array.length grid
    && y < Array.length grid.(0)
    && not (Char.equal grid.(x).(y) '#'))

let neighbours grid (x, y) =
  match grid.(x).(y) with
  | '>' -> [ (x, y + 1) ]
  | '<' -> [ (x, y - 1) ]
  | '^' -> [ (x - 1, y) ]
  | 'v' -> [ (x + 1, y) ]
  | _ -> adjacent grid (x, y)

let to_graph neighbours grid =
  let vertices = Hash_set.create (module Point) in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if not (Char.equal grid.(i).(j) '#')
      then (
        let pos = (i, j) in
        let next = neighbours grid pos in
        if List.length next > 2 then Hash_set.add vertices pos)
    done
  done;
  Hash_set.add vertices start;
  Hash_set.add vertices goal;
  let edges = Hashtbl.create (module Point) in
  (* BFS through each of the identified intersection to their next intersections
     identifying the cost *)
  Hash_set.iter vertices ~f:(fun v ->
    let q = Queue.create () in
    Queue.enqueue q (v, 0);
    let visited = Hash_set.create (module Point) in
    while not (Queue.is_empty q) do
      let p, cost = Queue.dequeue_exn q in
      if not (Hash_set.mem visited p)
      then (
        Hash_set.add visited p;
        if Hash_set.mem vertices p && not (Point.equal p v)
        then Hashtbl.add_multi edges ~key:v ~data:(p, cost)
        else neighbours grid p |> List.iter ~f:(fun n -> Queue.enqueue q (n, cost + 1)))
    done);
  edges

let dfs start goal graph =
  let seen = Hash_set.create (module Point) in
  let rec visit point cost =
    if Point.equal point goal
    then cost
    else if not (Hash_set.mem seen point)
    then (
      Hash_set.add seen point;
      let result =
        Hashtbl.find_multi graph point
        |> List.fold ~init:0 ~f:(fun acc (p, c) -> max acc (visit p (cost + c)))
      in
      Hash_set.remove seen point;
      result)
    else 0
  in
  visit start 0

let () =
  printf "start: %s\n%!" (Point.sexp_of_t start |> Sexp.to_string_hum);
  printf "goal: %s\n%!" (Point.sexp_of_t goal |> Sexp.to_string_hum);
  let time = Unix.gettimeofday () in
  to_graph neighbours lines |> dfs start goal |> printf "%d\n%!";
  printf "time: %fms\n%!" ((Unix.gettimeofday () -. time) *. 1000.);
  let time = Unix.gettimeofday () in
  to_graph adjacent lines |> dfs start goal |> printf "%d\n%!";
  printf "time: %fms\n%!" ((Unix.gettimeofday () -. time) *. 1000.)
