let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

type ball =
  | Red of int
  | Green of int
  | Blue of int

type shown_balls =
  { red : int
  ; green : int
  ; blue : int
  }

let to_total balls_shown =
  balls_shown
  |> List.fold_left
       (fun totals ball ->
         match ball with
         | Red count -> { totals with red = totals.red + count }
         | Green count -> { totals with green = totals.green + count }
         | Blue count -> { totals with blue = totals.blue + count })
       { red = 0; green = 0; blue = 0 }

let parse_line str =
  (* Example: "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  match String.split_on_char ':' str with
  | [ game; rest ] ->
    ( Scanf.sscanf game "Game %d" (fun g -> g)
    , rest
      |> String.split_on_char ';'
      |> List.map (fun turn ->
           turn
           |> String.split_on_char ','
           |> List.map (fun ball ->
                Scanf.sscanf (String.trim ball) "%d %s" (fun count colour ->
                  match colour with
                  | "red" -> Red count
                  | "green" -> Green count
                  | "blue" -> Blue count
                  | _ -> failwith "invalid colour")))
      |> List.map to_total )
  | _ -> failwith "invalid line"

let part1 () =
  let maxTotals = { red = 12; green = 13; blue = 14 } in
  lines
  |> Array.to_seq
  |> Seq.map parse_line
  |> Seq.filter_map (fun (game, totals) ->
       if totals
          |> List.for_all (fun total ->
               total.red <= maxTotals.red
               && total.green <= maxTotals.green
               && total.blue <= maxTotals.blue)
       then Some game
       else None)
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part1: %d\n"

let part2 () =
  lines
  |> Array.to_seq
  |> Seq.map parse_line
  |> Seq.map snd
  |> Seq.map (fun shown ->
       shown
       |> List.fold_left
            (fun total shown ->
              { red = max total.red shown.red
              ; green = max total.green shown.green
              ; blue = max total.blue shown.blue
              })
            { red = 0; green = 0; blue = 0 })
  |> Seq.map (fun total -> total.red * total.green * total.blue)
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part2: %d\n"

let () =
  part1 ();
  part2 ()
