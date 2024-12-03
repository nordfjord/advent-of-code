open Base
open Stdio

let tuple a b = (a, b)
let lines = In_channel.input_lines stdin
let parse = List.map ~f:(fun x -> Stdlib.Scanf.sscanf x "%d %d" tuple)

let incr_opt = function
  | None -> 1
  | Some x -> x + 1

let freq_tbl xs =
  let tbl = Hashtbl.create (module Int) in
  List.iter xs ~f:(Hashtbl.update tbl ~f:incr_opt);
  tbl

let part1 left right =
  List.zip_exn left right
  |> List.map ~f:(fun (a, b) -> abs (a - b))
  |> List.fold_left ~init:0 ~f:( + )

let part2 left right =
  let tbl = freq_tbl right in
  List.fold_left left ~init:0 ~f:(fun sum x ->
    match Hashtbl.find tbl x with
    | Some y -> sum + (x * y)
    | None -> sum)

let sort_int_list = List.sort ~compare:Int.compare

let () =
  let xs = parse lines in
  let left, right = List.unzip xs in
  let left = sort_int_list left in
  let right = sort_int_list right in
  part1 left right |> printf "Part 1: %d\n";
  part2 left right |> printf "Part 2: %d\n"
