open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.map ~f:String.to_array |> List.to_array

module IntInt = struct
  type t = int * int [@@deriving compare, sexp_of, hash]
end

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving show, compare, hash, sexp_of]

module Move = struct
  type t = direction * (int * int) [@@deriving show, compare, hash, sexp_of]
end

let next_moves dir c (x, y) =
  match (dir, c) with
  | Right, '|' | Left, '|' -> [ (Up, (x - 1, y)); (Down, (x + 1, y)) ]
  | Up, '-' | Down, '-' -> [ (Left, (x, y - 1)); (Right, (x, y + 1)) ]
  | Right, '\\' -> [ (Down, (x + 1, y)) ]
  | Left, '\\' -> [ (Up, (x - 1, y)) ]
  | Up, '\\' -> [ (Left, (x, y - 1)) ]
  | Down, '\\' -> [ (Right, (x, y + 1)) ]
  | Right, '/' -> [ (Up, (x - 1, y)) ]
  | Left, '/' -> [ (Down, (x + 1, y)) ]
  | Up, '/' -> [ (Right, (x, y + 1)) ]
  | Down, '/' -> [ (Left, (x, y - 1)) ]
  | Right, _ -> [ (Right, (x, y + 1)) ]
  | Left, _ -> [ (Left, (x, y - 1)) ]
  | Up, _ -> [ (Up, (x - 1, y)) ]
  | Down, _ -> [ (Down, (x + 1, y)) ]

let trace_path start arr =
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  let q = Queue.create () in
  Queue.enqueue q start;
  let visited = Hashtbl.create (module Move) in
  Hashtbl.add_exn visited ~key:start ~data:();
  while not (Queue.is_empty q) do
    let dir, (x, y) = Queue.dequeue_exn q in
    let c = arr.(x).(y) in
    let next = next_moves dir c (x, y) in
    List.iter next ~f:(fun (dir, (x, y)) ->
      if 0 <= x
         && x < rows
         && 0 <= y
         && y < cols
         && not (Hashtbl.mem visited (dir, (x, y)))
      then (
        Hashtbl.add_exn visited ~key:(dir, (x, y)) ~data:();
        Queue.enqueue q (dir, (x, y))))
  done;
  visited
  |> Hashtbl.to_alist
  |> List.map ~f:(fun ((_, (x, y)), _) -> (x, y))
  |> Set.Poly.of_list
  |> Set.length

(* part 1 *)
let () = lines |> trace_path (Right, (0, 0)) |> printf "%d\n"

let p2 arr =
  (* array is symmetric *)
  let l = Array.length arr in
  Sequence.range 0 l
  |> Sequence.concat_map ~f:(fun x ->
    [ (Right, (x, 0)); (Left, (x, l - 1)); (Down, (0, x)); (Up, (l - 1, x)) ]
    |> Sequence.of_list)
  |> Sequence.map ~f:(fun x -> trace_path x arr)
  |> Sequence.fold ~init:0 ~f:max

(* part 2 *)
let () =
  let now = Time_now.nanoseconds_since_unix_epoch () in
  p2 lines |> printf "%d\n";
  let elapsed = Int63.(Time_now.nanoseconds_since_unix_epoch () - now) in
  printf "running time: %fms\n" (Int63.to_float elapsed /. 1e6)
