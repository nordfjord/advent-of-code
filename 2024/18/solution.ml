open Base
open Stdio

module Point = struct
  type t = int * int [@@deriving sexp, compare, hash, equal, show]

  let up = (-1, 0)
  let down = (1, 0)
  let left = (0, -1)
  let right = (0, 1)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let rev (x, y) = (y, x)
end

let xmax, ymax = (70, 70)

let lines =
  In_channel.input_lines stdin
  |> List.map ~f:(fun s -> Stdlib.Scanf.sscanf s "%d,%d" (fun y x -> (x, y)))
  |> Array.of_list

let make_mem n =
  let result = Hash_set.create (module Point) in
  for i = 0 to n - 1 do
    Hash_set.add result lines.(i)
  done;
  result

let in_bounds (x, y) = 0 <= x && x <= xmax && 0 <= y && y <= ymax
let moves p = List.map [ Point.up; Point.down; Point.left; Point.right ] ~f:(Point.add p)

let part1 n =
  let memory = make_mem n in
  let q = Queue.create () in
  let visited = Hash_set.create (module Point) in
  Queue.enqueue q ((0, 0), 0);
  Hash_set.add visited (0, 0);
  let result = ref Int.max_value in
  while not (Queue.is_empty q) do
    let start, cost = Queue.dequeue_exn q in
    if Point.equal start (xmax, ymax) && cost < !result then result := cost;
    moves start
    |> List.filter ~f:(fun p -> in_bounds p && not (Hash_set.mem memory p))
    |> List.iter ~f:(fun p ->
      if Hash_set.mem visited p
      then ()
      else (
        Hash_set.add visited p;
        Queue.enqueue q (p, cost + 1)))
  done;
  !result

let rec part2 n =
  if part1 n <> Int.max_value then part2 (n + 1) else Point.rev lines.(n - 1)

let () =
  part1 1024 |> printf "Part 1: %d\n";
  part2 1025 |> Point.show |> printf "Part 2: %s\n"
