let readLines = System.IO.File.ReadAllLines

type Instr =
  | AddX of int
  | Noop

module Instr =
  let parse (s: string) =
    match s.Split(" ") with
    | [| "noop" |] -> Some Noop
    | [| "addx"; x |] -> Some (AddX (int x ))
    | _ -> None
  let toList = function
    | Noop -> [ 0 ]
    | AddX x -> [ 0 ; x ]

let part1 instructions =
  instructions
  |> Seq.collect Instr.toList
  |> Seq.scan (+) 1
  |> Seq.indexed
  |> Seq.map (fun (i, x) -> i + 1, x)
  |> Seq.take 221
  |> Seq.filter (fun (i, _) -> i = 20 || (i - 20) % 40 = 0)
  |> Seq.sumBy (fun (i,x) -> i * x)


let part2 instructions =
  instructions
  |> Seq.collect Instr.toList
  |> Seq.scan (+) 1
  |> Seq.mapi (fun i spritePosition ->
    let cursorPosition = i % 40
    if abs (spritePosition - cursorPosition) < 2 then
      '#'
    else
      '.')
  |> Seq.iteri (fun i c -> 
    if i % 40 = 0 then printf "\n"
    printf "%c" c
  )


let instructions =
  readLines "./input.txt"
  |> Array.choose Instr.parse

instructions
|> part1
|> (printfn "Part1: %A")

instructions
|> part2
