let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> List.of_seq

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let parse_card str =
  let open Angstrom in
  let line =
    let integer =
      take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
      >>| int_of_string
    in
    let ws =
      skip_while (function
        | ' ' -> true
        | _ -> false)
    in
    let* _ = string "Card" <* ws <* integer <* char ':' <* ws in
    let* winning = sep_by1 ws integer <* ws <* char '|' <* ws in
    let* mine = sep_by1 ws integer <* ws in
    return (IntSet.of_list winning, IntSet.of_list mine)
  in
  match parse_string ~consume:All line str with
  | Ok (winning, mine) -> (winning, mine)
  | Error msg -> failwith msg

let cards = List.map parse_card lines

let score = function
  | 0 -> 0
  (* lsl = left shift logical, doubling via bit hackery *)
  | n -> 1 lsl (n - 1)

let play (winning, mine) = IntSet.inter winning mine |> IntSet.to_seq |> Seq.length

let part1 () =
  cards
  |> List.map play
  |> List.map score
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n"

let play_part2 cards_held card_number cards =
  let current_count = cards_held |> IntMap.find card_number in
  let score = play cards in
  Seq.ints 1
  |> Seq.take score
  |> Seq.map (fun x -> x + card_number)
  |> Seq.filter (fun x -> IntMap.mem x cards_held)
  |> Seq.fold_left
       (fun acc x ->
         let curr = IntMap.find x acc in
         IntMap.add x (curr + current_count) acc)
       cards_held

let calc_score cards = IntMap.fold (fun _ v acc -> acc + v) cards 0

let part2 () =
  let len = List.length cards in
  let initial =
    Seq.ints 0 |> Seq.take len |> Seq.map (fun x -> (x, 1)) |> IntMap.of_seq
  in
  cards
  |> List.to_seq
  |> Seq.mapi (fun i (winning, mine) -> (i, (winning, mine)))
  |> Seq.fold_left (fun acc (i, cards) -> play_part2 acc i cards) initial
  |> calc_score
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
