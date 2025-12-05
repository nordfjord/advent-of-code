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

let adjacent (x, y) =
  [ (x - 1, y + 1) ;  (x, y + 1) ; (x + 1, y + 1)
  ; (x - 1, y    ) ;  (*(x, y)*)   (x + 1, y    )
  ; (x - 1, y - 1) ;  (x, y - 1) ; (x + 1, y - 1)
  ] [@@ocamlformat "disable"]

let find_removable grid =
  Hash_set.filter grid ~f:(fun coords ->
    adjacent coords |> List.count ~f:(fun coords -> Hash_set.mem grid coords) < 4)

let part1 grid = find_removable grid |> Hash_set.length

let part2 grid =
  let rec aux count grid =
    let removable = find_removable grid in
    if Hash_set.length removable = 0
    then count
    else aux (count + Hash_set.length removable) (Hash_set.diff grid removable)
  in
  aux 0 grid

let () =
  part1 grid |> printf "Part 1: %d\n";
  part2 grid |> printf "Part 2: %d\n"
