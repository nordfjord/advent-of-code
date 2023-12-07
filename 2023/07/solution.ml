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

  let to_char = function
    | 14 -> 'A'
    | 13 -> 'K'
    | 12 -> 'Q'
    | 11 -> 'J'
    | 10 -> 'T'
    | c -> char_of_int (c + int_of_char '0')

  let of_string s = String.to_seq s |> Seq.map of_char |> List.of_seq
  let to_chars = Array.map to_char
  let to_string h = h |> List.map to_char |> List.to_seq |> String.of_seq

  let rank hand =
    let cards =
      hand
      |> List.sort compare
      |> List.to_seq
      |> Seq.group ( = )
      |> Seq.map Seq.length
      |> List.of_seq
      |> List.sort (fun a b -> -compare a b)
    in
    match cards with
    | [ 5 ] -> 6
    | [ 4; _ ] -> 5
    | [ 3; 2 ] -> 4
    | 3 :: _ -> 3
    | 2 :: 2 :: _ -> 2
    | 2 :: _ -> 1
    | _ -> 0

  let rec compare_hand h1 h2 =
    match (h1, h2) with
    | [], [] -> 0
    | a :: _, b :: _ when a > b -> 1
    | a :: _, b :: _ when a < b -> -1
    | _ :: xs, _ :: ys -> compare_hand xs ys
    | _ -> failwith "impossible"

  let compare hand1 hand2 =
    let rank1 = rank hand1 in
    let rank2 = rank hand2 in
    if rank1 > rank2 then 1 else if rank1 < rank2 then -1 else compare_hand hand1 hand2
end

let parse line =
  let [ hand; bid ] = line |> String.split_on_char ' ' in
  (Hand.of_string hand, int_of_string bid)
[@@warning "-8"]

type parsed = Hand.t * int [@@deriving show]

let hands = List.map parse lines
let ( >> ) f g x = g (f x)

let tap f x =
  f x;
  x

let part1 () =
  hands
  |> List.sort (fun (h1, _) (h2, _) -> Hand.compare h1 h2)
  |> List.map (tap (show_parsed >> print_endline))
  |> List.mapi (fun i x -> (i + 1, x))
  |> List.fold_left
       (fun acc (i, (h, bid)) ->
         Printf.printf "%d %s %d\n" i (Hand.to_string h) bid;
         acc + (i * bid))
       0
  |> Printf.printf "Part 1: %d\n"

let () = part1 ()
