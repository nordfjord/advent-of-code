open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse lines =
  lines |> List.map ~f:(fun x -> Stdlib.Scanf.sscanf x "%d %d" (fun a b -> (a, b)))

let freq_tbl xs =
  let incr = function
    | None -> 1
    | Some x -> x + 1
  in
  let tbl = Hashtbl.create (module Int) in
  List.iter xs ~f:(fun x -> Hashtbl.update tbl x ~f:incr);
  tbl

let part1 left right =
  List.zip_exn left right
  |> List.fold_left ~init:0 ~f:(fun sum (a, b) -> sum + abs (a - b))

let part2 left right =
  let tbl = freq_tbl right in
  List.fold_left left ~init:0 ~f:(fun sum x ->
    match Hashtbl.find tbl x with
    | Some y -> sum + (x * y)
    | None -> sum)

let () =
  let xs = parse lines in
  let left, right = List.unzip xs in
  let left = List.sort ~compare:Int.compare left in
  let right = List.sort ~compare:Int.compare right in
  part1 left right |> printf "Part 1: %d\n";
  part2 left right |> printf "Part 2: %d\n"
