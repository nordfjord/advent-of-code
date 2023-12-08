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
    |> Seq.map (fun s -> Scanf.sscanf s "%s = (%s@, %s@)" (fun src l r -> (src, (l, r))))
    |> Hashtbl.of_seq
  in
  (directions, graph)

let directions, graph = parse lines

let part1 directions graph =
  let rec aux curr i =
    let dir = directions.(i mod Array.length directions) in
    let l, r = Hashtbl.find graph curr in
    let next =
      match dir with
      | Left -> l
      | Right -> r
    in
    match next with
    | "ZZZ" -> i + 1
    | _ -> aux next (i + 1)
  in
  aux "AAA" 0

let rec solve start i directions graph visited =
  let dir = directions.(i mod Array.length directions) in
  let l, r = Hashtbl.find graph start in
  let next =
    match dir with
    | Left -> l
    | Right -> r
  in
  match visited with
  | (_, z) :: _ when next = z -> visited
  | _ when next |> String.ends_with ~suffix:"Z" ->
    solve next (i + 1) directions graph ((i + 1, next) :: visited)
  | _ -> solve next (i + 1) directions graph visited

let lcd a b =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  a * b / gcd a b

let rec part2 directions graph =
  let starting_points =
    graph
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun x -> x |> String.ends_with ~suffix:"A")
    |> List.of_seq
  in
  let cycles = starting_points |> List.map (fun s -> solve s 0 directions graph []) in
  cycles |> List.fold_left (fun acc l -> lcd acc (List.hd l |> fst)) 1

let () =
  part1 directions graph |> Printf.printf "Part 1: %d\n";
  part2 directions graph |> Printf.printf "Part 2: %d\n"
