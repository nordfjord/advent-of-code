let part1 (input: string) =
    let positions = input.Split(",") |> Seq.map int
    let uniq = positions |> Seq.distinct

    uniq
    |> Seq.map (fun target ->
        positions
        |> Seq.fold (fun fuel pos -> fuel + (abs (target - pos))) 0)
    |> Seq.min

// Easy target for memoize
let fuelCost n = seq { 1 .. n } |> Seq.sum

let part2 (input: string) =
    let positions = input.Split(",") |> Seq.map int
    let max = positions |> Seq.max

    let endpoints = seq { 0 .. max }

    endpoints
    |> Seq.map (fun target ->
        positions
        |> Seq.fold
            (fun fuel pos ->
                let distance = abs (target - pos)
                let cost = fuelCost distance
                fuel + cost)
            0)
    |> Seq.min




let readLines filePath = System.IO.File.ReadAllText(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "Test"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "Actual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
