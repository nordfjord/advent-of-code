open Base
open Stdio
open Poly

let grid =
  In_channel.input_lines stdin
  |> List.map ~f:(fun xs ->
    String.to_array xs |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))
  |> Array.of_list

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, hash, compare]

  let turn_left = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

  let turn_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let translate dir (x, y) =
    match dir with
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)
    | Left -> (x, y - 1)
    | Right -> (x, y + 1)
end

type state =
  { coords : int * int
  ; cost : int
  ; direction : Direction.t
  ; consecutive : int
  }
[@@deriving sexp, hash, compare]

module State = struct
  type t = state [@@deriving sexp, hash, compare]

  let key state = (state.coords, state.consecutive, state.direction)
end

let in_bounds grid (x, y) =
  0 <= x && x < Array.length grid && 0 <= y && y < Array.length grid.(0)

let left grid state =
  let dir = Direction.turn_left state.direction in
  let x, y = Direction.translate dir state.coords in
  if in_bounds grid (x, y)
  then (
    let cost = state.cost + grid.(x).(y) in
    Some { coords = (x, y); direction = dir; cost; consecutive = 1 })
  else None

let right grid state =
  let dir = Direction.turn_right state.direction in
  let x, y = Direction.translate dir state.coords in
  if in_bounds grid (x, y)
  then (
    let cost = state.cost + grid.(x).(y) in
    Some { coords = (x, y); direction = dir; cost; consecutive = 1 })
  else None

let continue grid state =
  let x, y = Direction.translate state.direction state.coords in
  if in_bounds grid (x, y) && state.consecutive < 3
  then (
    let cost = state.cost + grid.(x).(y) in
    Some
      { coords = (x, y)
      ; direction = state.direction
      ; cost
      ; consecutive = state.consecutive + 1
      })
  else None

let bfs grid start =
  let q = Queue.create () in
  let visited = Hashtbl.Poly.create () in
  Queue.enqueue q { coords = start; direction = Right; cost = 0; consecutive = 0 };
  Queue.enqueue q { coords = start; direction = Down; cost = 0; consecutive = 0 };
  let result = ref Int.max_value in
  while not (Queue.is_empty q) do
    let state = Queue.dequeue_exn q in
    if state.coords = (Array.length grid - 1, Array.length grid.(0) - 1)
    then (
      result := min state.cost !result;
      printf "Path: cost=%d\n%!" state.cost);
    [ left grid state; right grid state; continue grid state ]
    |> List.filter_map ~f:Fn.id
    |> List.filter ~f:(fun s ->
      match Hashtbl.find visited (State.key s) with
      | None -> true
      | Some cost -> cost > s.cost)
    |> List.iter ~f:(fun s ->
      Hashtbl.set visited ~key:(State.key s) ~data:s.cost;
      Queue.enqueue q s)
  done;
  !result

let () =
  let start = (0, 0) in
  let cost = bfs grid start in
  printf "%d\n" cost
