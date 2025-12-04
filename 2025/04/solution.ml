open Base
open Stdio

let lines = In_channel.input_lines stdin

module IntPair = struct
  type t = int * int [@@deriving compare, sexp, equal, hash]
end

let grid = Hash_set.create (module IntPair)

let () =
  List.iteri lines ~f:(fun y str ->
    String.iteri str ~f:(fun x c ->
      match c with
      | '@' -> Hash_set.add grid (x, y)
      | _ -> ()))

let moves = [ (1, 1); (1, 0); (1, -1); (0, 1); (0, -1); (-1, 1); (-1, 0); (-1, -1) ]
let adjacent (x, y) = moves |> List.map ~f:(fun (x2, y2) -> (x + x2, y + y2))

let part1 grid =
  Hash_set.filter grid ~f:(fun coords ->
    adjacent coords |> List.count ~f:(fun coords -> Hash_set.mem grid coords) < 4)

let part2 grid =
  let rec aux count grid =
    let removable = part1 grid in
    if Hash_set.length removable = 0
    then count
    else aux (count + Hash_set.length removable) (Hash_set.diff grid removable)
  in
  aux 0 grid

let () =
  part1 grid |> Hash_set.length |> printf "Part 1: %d\n";
  part2 grid |> printf "Part 2: %d\n"
