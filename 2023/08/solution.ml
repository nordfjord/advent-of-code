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

type curr = string list [@@deriving show]
type cycles = (int * string) list list [@@deriving show]

let rec solve start i directions graph visited =
  let dir = directions.(i mod Array.length directions) in
  let l, r = Hashtbl.find graph start in
  let next =
    match dir with
    | Left -> l
    | Right -> r
  in
  match visited with
  | (_, z) :: _ when next = z ->
    Printf.printf "Found cycle: (%d, %s)\n%!" i next;
    visited
  | _ when next |> String.ends_with ~suffix:"Z" ->
    Printf.printf "Found end: (%d, %s)\n%!" i next;
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
  Printf.printf "Starting points: %s\n%!" (show_curr starting_points);
  let cycles = starting_points |> List.map (fun s -> solve s 0 directions graph []) in
  Printf.printf "%s\n" (show_cycles cycles);
  cycles |> List.fold_left (fun acc l -> lcd acc (List.hd l |> fst)) 1

let () = part2 directions graph |> Printf.printf "Part 2: %d\n"
