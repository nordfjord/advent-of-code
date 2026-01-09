open Stdio
open Prelude

let points =
  In_channel.input_lines stdin
  |> Base.List.concat_mapi ~f:(fun y s ->
    Base.String.to_list s
    |> Base.List.filter_mapi ~f:(fun x c ->
      match c with
      | '#' -> Some (x, y)
      | _ -> None))
  |> Array.of_list

let part1 () =
  points
  |> Array.to_seq
  |> Seq.map (fun p ->
    points
    |> Array.to_seq
    |> Seq.filter (fun point -> point <> p)
    |> Seq.map (Point.angle p)
    |> List.of_seq
    |> List.sort_uniq compare
    |> List.length)
  |> Seq.fold_left max 0
  |> printf "Part1: %d\n"

let score (x, y) = (100 * x) + y

let part2 () =
  let starting_point, _ =
    points
    |> Array.to_seq
    |> Seq.map (fun p ->
      ( p
      , points
        |> Array.to_seq
        |> Seq.filter (fun point -> point <> p)
        |> Seq.map (Point.angle p)
        |> List.of_seq
        |> List.sort_uniq compare
        |> List.length ))
    |> Seq.fold_left
         (fun (p, seen) (point, value) ->
            if value > seen then (point, value) else (p, seen))
         ((0, 0), 0)
  in
  let point =
    points
    |> Array.to_seq
    |> Seq.filter (( <> ) starting_point)
    |> Seq.map (fun p -> (p, Point.angle starting_point p))
    |> List.of_seq
    |> List.sort (fun a b ->
      let a = snd a in
      let b = snd b in
      compare a b)
    |> List.to_seq
    |> Seq.group (fun a b -> snd a = snd b)
    |> Seq.transpose
    |> Seq.concat
    |> Seq.map fst
    |> Seq.drop 199
    |> Seq.take 1
  in
  point |> Seq.map score |> Seq.iter (printf "Part 2: %d\n")

let () =
  part1 ();
  part2 ()
