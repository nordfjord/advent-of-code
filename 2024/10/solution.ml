open Base
open Stdio

module IntInt = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module IntIntList = struct
  type t = IntInt.t list [@@deriving sexp, compare, hash]
end

let int_of_char c = Stdlib.int_of_char c - Stdlib.int_of_char '0'

let map =
  In_channel.input_lines stdin
  |> List.to_array
  |> Array.map ~f:(fun s -> String.to_array s |> Array.map ~f:int_of_char)

let max_x = Array.length map - 1
let max_y = Array.length map.(0) - 1
let in_bounds (x, y) = 0 <= x && x <= max_x && 0 <= y && y <= max_y
let resolve (x, y) = map.(x).(y)

let moves (x, y) h =
  [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
  |> List.filter ~f:(fun p -> in_bounds p && resolve p = h + 1)

let part1 (x, y) =
  let q = Queue.create () in
  Queue.enqueue q ((x, y), resolve (x, y));
  let tops = Hash_set.create (module IntInt) in
  let visited = Hash_set.create (module IntInt) in
  while not (Queue.is_empty q) do
    let point, height = Queue.dequeue_exn q in
    if height = 9
    then Hash_set.add tops point
    else
      moves point height
      |> List.iter ~f:(fun p ->
        let h = resolve p in
        if Hash_set.mem visited p
        then ()
        else (
          Hash_set.add visited p;
          Queue.enqueue q (p, h)))
  done;
  Hash_set.length tops

let part2 (x, y) =
  let q = Queue.create () in
  Queue.enqueue q ((x, y), resolve (x, y), []);
  let paths = Hash_set.create (module IntIntList) in
  let visited = Hash_set.create (module IntIntList) in
  while not (Queue.is_empty q) do
    let point, height, path = Queue.dequeue_exn q in
    if height = 9
    then Hash_set.add paths path
    else
      moves point height
      |> List.iter ~f:(fun p ->
        let next_path = p :: path in
        let h = resolve p in
        if Hash_set.mem visited next_path
        then ()
        else (
          Hash_set.add visited next_path;
          Queue.enqueue q (p, h, next_path)))
  done;
  Hash_set.length paths

let trailheads =
  let result = ref [] in
  for x = 0 to max_x do
    for y = 0 to max_y do
      if resolve (x, y) = 0 then result := (x, y) :: !result
    done
  done;
  !result

let () =
  trailheads |> List.sum (module Int) ~f:part1 |> printf "Part 1: %d\n";
  trailheads |> List.sum (module Int) ~f:part2 |> printf "Part 2: %d\n"
