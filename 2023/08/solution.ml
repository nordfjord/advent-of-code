let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

type direction =
  | Left
  | Right

let parse input =
  let directions =
    input.(0)
    |> String.to_seq
    |> Seq.map (function
      | 'L' -> Left
      | 'R' -> Right
      | _ -> failwith "invalid direction")
    |> Array.of_seq
  in
  let graph =
    Array.to_seq input
    |> Seq.drop 2
    |> Seq.map (fun s ->
        Printf.printf "%s\n" s;
        Scanf.sscanf s "%s = (%s@, %s@)" (fun src l r -> (src, (l, r))))
    |> Hashtbl.of_seq
  in
  (directions, graph)

let directions, graph = parse lines

let solve directions graph =
  let current = ref "AAA" in
  Seq.ints 0
  |> Seq.map (fun i ->
    let dir = directions.(i mod Array.length directions) in
    let l, r = Hashtbl.find graph !current in
    (current
     := match dir with
        | Left -> l
        | Right -> r);
    !current)
  |> Seq.take_while (fun s -> s <> "ZZZ")
  |> Seq.length
  |> (+) 1

let () = solve directions graph |> Printf.printf "Part 1: %d\n"
