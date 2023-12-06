open Printf
module IntSet = Set.Make (Int)

let parse_line r =
  Scanf.sscanf r "%d-%d,%d-%d" (fun x1 y1 x2 y2 ->
    let left = Seq.ints x1 |> Seq.take (y1 - x1 + 1) |> IntSet.of_seq in
    let right = Seq.ints x2 |> Seq.take (y2 - x2 + 1) |> IntSet.of_seq in
    (left, right))

let engulfs (a, b) = IntSet.subset b a || IntSet.subset a b
let intersects (a, b) = a |> IntSet.inter b |> IntSet.cardinal > 0

let () =
  let lines =
    Seq.of_dispenser (fun _ ->
      match read_line () with
      | x -> Some x
      | exception End_of_file -> None)
    |> Array.of_seq
    |> Array.to_seq
    |> Seq.map parse_line
  in
  lines |> Seq.filter engulfs |> Seq.length |> printf "Part1: %d\n";
  lines |> Seq.filter intersects |> Seq.length |> printf "Part 2: %d\n"
