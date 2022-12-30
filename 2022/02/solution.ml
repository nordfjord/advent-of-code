open Printf

module Play = struct
  type t = Rock | Paper | Scissors

  let of_string = function
    | "A" | "X" ->
        Some Rock
    | "B" | "Y" ->
        Some Paper
    | "C" | "Z" ->
        Some Scissors
    | _ ->
        None

  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let play = function
    | Rock, Rock ->
        3
    | Rock, Paper ->
        0
    | Rock, Scissors ->
        6
    | Paper, Rock ->
        6
    | Paper, Paper ->
        3
    | Paper, Scissors ->
        0
    | Scissors, Rock ->
        0
    | Scissors, Paper ->
        6
    | Scissors, Scissors ->
        3

  let score (opponent, me) = score me + play (me, opponent)
end

module DesiredOutcome = struct
  type t = Lose | Draw | Win

  let of_string = function
    | "X" ->
        Some Lose
    | "Y" ->
        Some Draw
    | "Z" ->
        Some Win
    | _ ->
        None

  let play (opponent, me) =
    let open Play in
    let myChoice =
      match (opponent, me) with
      | _, Draw ->
          opponent
      | Rock, Lose ->
          Scissors
      | Paper, Lose ->
          Rock
      | Scissors, Lose ->
          Paper
      | Rock, Win ->
          Paper
      | Paper, Win ->
          Scissors
      | Scissors, Win ->
          Rock
    in
    score (opponent, myChoice)
end

let () =
  let lines =
    Seq.of_dispenser (fun _ ->
        match read_line () with x -> Some x | exception End_of_file -> None )
    |> List.of_seq
  in
  lines |> List.to_seq
  |> Seq.map (String.split_on_char ' ')
  |> Seq.filter_map (function
       | a :: b :: _ -> (
         match (Play.of_string a, Play.of_string b) with
         | Some a, Some b ->
             Some (a, b)
         | _ ->
             None )
       | _ ->
           None )
  |> Seq.map Play.score |> Seq.fold_left ( + ) 0 |> printf "Part 1: %d\n" ;
  lines |> List.to_seq
  |> Seq.map (String.split_on_char ' ')
  |> Seq.filter_map (function
       | a :: b :: _ -> (
         match (Play.of_string a, DesiredOutcome.of_string b) with
         | Some a, Some b ->
             Some (a, b)
         | _ ->
             None )
       | _ ->
           None )
  |> Seq.map DesiredOutcome.play
  |> Seq.fold_left ( + ) 0 |> printf "Part 2: %d\n"
