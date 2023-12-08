let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let parse input =
  let directions =
    input.(0)
    |> String.to_seq
    |> Seq.map (function
      | 'L' -> fst
      | 'R' -> snd
      | _ -> failwith "invalid direction")
    |> Array.of_seq
  in
  let graph =
    Array.to_seq input
    |> Seq.drop 2
    |> Seq.map (fun s -> Scanf.sscanf s "%s = (%s@, %s@)" (fun src l r -> (src, (l, r))))
    |> Hashtbl.of_seq
  in
  (directions, graph)

let directions, graph = parse lines

let part1 directions graph =
  let rec aux curr i =
    let dir = directions.(i mod Array.length directions) in
    let next = Hashtbl.find graph curr |> dir in
    match next with
    | "ZZZ" -> i + 1
    | _ -> aux next (i + 1)
  in
  aux "AAA" 0

let lcm a b =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  a * b / gcd a b

let part2 directions graph =
  (* It turned out that each starting point only had a single endpoint *)
  let rec aux curr i visited =
    let dir = directions.(i mod Array.length directions) in
    let next = Hashtbl.find graph curr |> dir in
    match visited with
    | Some (j, s) when s = next -> j
    | _ when next |> String.ends_with ~suffix:"Z" -> aux next (i + 1) (Some (i + 1, next))
    | _ -> aux next (i + 1) visited
  in
  let starting_points =
    graph
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun x -> x |> String.ends_with ~suffix:"A")
    |> List.of_seq
  in
  let cycles = starting_points |> List.map (fun s -> aux s 0 None) in
  cycles |> List.fold_left (fun acc x -> lcm acc x) 1

let () =
  part1 directions graph |> Printf.printf "Part 1: %d\n";
  part2 directions graph |> Printf.printf "Part 2: %d\n"
