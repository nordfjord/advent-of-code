type Play =
  | Rock
  | Paper
  | Scissors

type DesiredOutcome =
  | Lose
  | Draw
  | Win

module DesiredOutcome =
  let fromString =
    function
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None

module Play =
  let fromString =
    function
    | "A"
    | "X" -> Some Rock
    | "B"
    | "Y" -> Some Paper
    | "C"
    | "Z" -> Some Scissors
    | _ -> None

  let score =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

  let play =
    function
    | Rock, Rock -> 3
    | Rock, Paper -> 0
    | Rock, Scissors -> 6
    | Paper, Rock -> 6
    | Paper, Paper -> 3
    | Paper, Scissors -> 0
    | Scissors, Rock -> 0
    | Scissors, Paper -> 6
    | Scissors, Scissors -> 3

  let calcScoreDirect (opponent, me) = score me + play (me, opponent)

  let withStrategy (opponent, me) =
    let myChoice =
      match opponent, me with
      | _, Draw -> opponent
      | Rock, Lose -> Scissors
      | Paper, Lose -> Rock
      | Scissors, Lose -> Paper
      | Rock, Win -> Paper
      | Paper, Win -> Scissors
      | Scissors, Win -> Rock

    calcScoreDirect (opponent, myChoice)

module Array =
  let toTuple =
    function
    | [| a; b |] -> a, b
    | _ -> failwith "Array does not contain exactly two elements"

module Tuple =
  let map f g (a, b) = f a, g b

  let keepSome =
    function
    | Some a, Some b -> Some(a, b)
    | _ -> None



let readLines = System.IO.File.ReadAllLines

let split (sep: string) (s: string) = s.Split(sep)

let part1 s =
  split " " s
  |> Array.choose Play.fromString
  |> Array.toTuple
  |> Play.calcScoreDirect

readLines "./input.txt" |> Array.sumBy part1 |> printfn "Part 1: %i"


let part2 s =
  split " " s
  |> Array.toTuple
  |> Tuple.map Play.fromString DesiredOutcome.fromString
  |> Tuple.keepSome
  |> Option.get
  |> Play.withStrategy

readLines "./input.txt" |> Array.sumBy part2 |> printfn "Part 2: %i"
