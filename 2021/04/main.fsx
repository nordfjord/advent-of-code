type BingoCard =
    { Rows: Set<int> list
      Cols: Set<int> list }


type ParsedInput =
    { Draws: int []
      Cards: BingoCard list }

let transpose (matrix: _ [] []) =
    Array.init matrix.[0].Length (fun i -> Array.init matrix.Length (fun j -> matrix.[j].[i]))

open System.Text.RegularExpressions

let parseInput (input: string) =
    let inputs = input.Split("\n\n")
    let draws = inputs.[0].Split(",") |> Array.map int

    let cards =
        inputs
        |> Array.skip 1
        |> Array.map (fun cardStr ->
            cardStr.Split("\n")
            |> Array.map (fun lineStr ->
                let split = Regex(@"\W+").Split(lineStr.Trim())
                split |> Array.map int))
        |> Array.map (fun matrix ->
            { Rows = matrix |> Array.map Set.ofArray |> Array.toList
              Cols =
                transpose matrix
                |> Array.map Set.ofArray
                |> Array.toList })
        |> Array.toList

    { Draws = draws; Cards = cards }



let part1 input =
    let parsed = parseInput input

    let mutable nums =
        parsed.Draws |> Array.take 4 |> Set.ofArray

    let mutable winner = None

    let hasWon nums card =
        card.Cols
        |> List.exists (fun col -> Set.isSubset col nums)
        || card.Rows
           |> List.exists (fun row -> Set.isSubset row nums)

    let findWinner nums =
        parsed.Cards |> List.tryFind (hasWon nums)


    while winner = None do
        let len = nums.Count
        nums <- nums |> Set.add parsed.Draws.[len]
        winner <- findWinner nums

    let (Some winner) = winner

    let winningNumbers =
        winner.Rows |> List.fold Set.union Set.empty

    let rowSum =
        Set.difference winningNumbers nums
        |> Set.toList
        |> List.sum

    let lastNum = parsed.Draws.[nums.Count - 1]

    rowSum * lastNum



let part2 input =
    let parsed = parseInput input

    let mutable nums =
        parsed.Draws |> Array.take 4 |> Set.ofArray

    let mutable loser = None

    let mutable cards = parsed.Cards

    let hasWon nums card =
        card.Cols
        |> List.exists (fun col -> Set.isSubset col nums)
        || card.Rows
           |> List.exists (fun col -> Set.isSubset col nums)

    let findWinner nums = cards |> List.tryFind (hasWon nums)


    while loser = None do
        let len = nums.Count
        let draw = parsed.Draws.[len]
        nums <- nums |> Set.add draw

        if cards.Length = 1 then
            if hasWon nums cards.[0] then
                loser <- Some cards.[0]
        else
            cards <- cards |> List.filter (hasWon nums >> not)


    let (Some loser) = loser

    let losingNumbers =
        loser.Cols |> List.fold Set.union Set.empty

    let rowSum =
        Set.difference losingNumbers nums
        |> Set.toList
        |> List.sum

    let lastNum = parsed.Draws.[nums.Count - 1]

    rowSum * lastNum



let readLines filePath = System.IO.File.ReadAllText(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "Test"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "Actual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
