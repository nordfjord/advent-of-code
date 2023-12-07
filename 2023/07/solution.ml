let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> List.of_seq

module Hand = struct
  type t = int list [@@deriving show]

  let of_char = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> int_of_char c - int_of_char '0'

  let of_string s = String.to_seq s |> Seq.map of_char |> List.of_seq

  let compare = compare
end

module Part1 = struct
  let group_by_card hand =
    hand
    |> List.sort compare
    |> List.to_seq
    |> Seq.group ( = )
    |> Seq.map Seq.length
    |> List.of_seq
    |> List.sort (fun a b -> b - a)

  let rank_grouped = function
    | [ 5 ] -> 6
    | [ 4; _ ] -> 5
    | [ 3; 2 ] -> 4
    | 3 :: _ -> 3
    | 2 :: 2 :: _ -> 2
    | 2 :: _ -> 1
    | _ -> 0

  let rank hand = group_by_card hand |> rank_grouped

  let compare hand1 hand2 =
    let rank1 = rank hand1 in
    let rank2 = rank hand2 in
    if rank1 > rank2 then 1 else if rank1 < rank2 then -1 else Hand.compare hand1 hand2
end

module Part2 = struct
  type t = int list [@@deriving show]

  let transform_jacks = List.map (fun x -> if x = 11 then -1 else x)

  let add_jacks jacks = function
    | [] -> [ jacks ]
    | x :: xs -> (x + jacks) :: xs

  let rank hand =
    let jacks, no_jacks = hand |> List.partition (fun x -> x = -1) in
    no_jacks |> Part1.group_by_card |> add_jacks (List.length jacks) |> Part1.rank_grouped

  let compare hand1 hand2 =
    let hand1 = transform_jacks hand1 in
    let hand2 = transform_jacks hand2 in
    let rank1 = rank hand1 in
    let rank2 = rank hand2 in
    if rank1 > rank2 then 1 else if rank1 < rank2 then -1 else Hand.compare hand1 hand2
end

let parse line =
  let [ hand; bid ] = line |> String.split_on_char ' ' in
  (Hand.of_string hand, int_of_string bid)
[@@warning "-8"]

type parsed = Hand.t * int [@@deriving show]

let hands = List.map parse lines

let part1 () =
  hands
  |> List.sort (fun (h1, _) (h2, _) -> Part1.compare h1 h2)
  |> List.mapi (fun i x -> (i + 1, snd x))
  |> List.fold_left (fun acc (i, bid) -> acc + (i * bid)) 0
  |> Printf.printf "Part 1: %d\n"

let part2 () =
  hands
  |> List.sort (fun (h1, _) (h2, _) -> Part2.compare h1 h2)
  |> List.mapi (fun i x -> (i + 1, snd x))
  |> List.fold_left (fun acc (i, bid) -> acc + (i * bid)) 0
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
