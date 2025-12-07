open Base
open Stdio

let lines = In_channel.input_lines stdin

let start, splitters =
  List.foldi
    lines
    ~init:((0, 0), [])
    ~f:(fun y (start, splitters) line ->
      String.foldi line ~init:(start, splitters) ~f:(fun x (start, splitters) c ->
        match c with
        | 'S' -> ((x, y), splitters)
        | '^' -> (start, (x, y) :: splitters)
        | _ -> (start, splitters)))

module IntPair = struct
  type t = int * int [@@deriving compare, hash, sexp]
end

let splitters = Hash_set.of_list (module IntPair) splitters
let max_y = List.length lines - 1
let width = String.length (List.hd_exn lines)

let solve (x, start_y) =
  let paths = Array.create ~len:width 0 in
  let splits = ref 0 in
  paths.(x) <- 1;
  for y = start_y to max_y do
    for x = 0 to width - 1 do
      if paths.(x) > 0 && Hash_set.mem splitters (x, y)
      then (
        Int.incr splits;
        paths.(x - 1) <- paths.(x - 1) + paths.(x);
        paths.(x + 1) <- paths.(x + 1) + paths.(x);
        paths.(x) <- 0)
    done
  done;
  (!splits, Array.fold paths ~init:0 ~f:( + ))

let () =
  let p1, p2 = solve start in
  printf "Part 1: %d\n" p1;
  printf "Part 2: %d\n" p2
