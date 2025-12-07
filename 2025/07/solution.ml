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

let part1 start =
  let visited = Hash_set.create (module IntPair) in
  let rec aux (x, y) =
    if y >= max_y || Hash_set.mem visited (x, y)
    then 0
    else (
      let res =
        if Hash_set.mem splitters (x, y)
        then 1 + aux (x - 1, y + 1) + aux (x + 1, y + 1)
        else aux (x, y + 1)
      in
      Hash_set.add visited (x, y);
      res)
  in
  aux start

let part2 start =
  let memo = Hashtbl.create (module IntPair) in
  let rec aux (x, y) =
    if y >= max_y
    then 1
    else (
      match Hashtbl.find memo (x, y) with
      | Some v -> v
      | None ->
        let paths =
          if Hash_set.mem splitters (x, y)
          then aux (x - 1, y + 1) + aux (x + 1, y + 1)
          else aux (x, y + 1)
        in
        Hashtbl.set memo ~key:(x, y) ~data:paths;
        paths)
  in
  aux start

let () =
  part1 start |> printf "Part 1: %d\n";
  part2 start |> printf "Part 2: %d\n"
