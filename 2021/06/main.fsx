let empty: Map<int64, int64> =
    Map.ofList [ (0L, 0L)
                 (1L, 0L)
                 (2L, 0L)
                 (3L, 0L)
                 (4L, 0L)
                 (5L, 0L)
                 (6L, 0L)
                 (7L, 0L)
                 (8L, 0L) ]

let incrementKey map k =
    map
    |> Map.change k (function
        | Some v -> Some(v + 1L)
        | None -> Some 1L)

let parseInput (s: string) =
    s.Split(",")
    |> Array.map int64
    |> Array.fold incrementKey empty


let simulateDay map =
    map
    |> Map.add 0L (Map.find 1L map)
    |> Map.add 1L (Map.find 2L map)
    |> Map.add 2L (Map.find 3L map)
    |> Map.add 3L (Map.find 4L map)
    |> Map.add 4L (Map.find 5L map)
    |> Map.add 5L (Map.find 6L map)
    |> Map.add 6L ((Map.find 7L map) + (Map.find 0L map))
    |> Map.add 7L (Map.find 8L map)
    |> Map.add 8L (Map.find 0L map)

let rec simulateDays count map =
    match count with
    | 0 -> map
    | n -> simulateDays (n - 1) (simulateDay map)

let part1 input =
    parseInput input
    |> simulateDays 80
    |> Map.fold (fun count k v -> count + v) 0L

let part2 input =
    parseInput input
    |> simulateDays 256
    |> Map.fold (fun count k v -> count + v) 0L

let readLines filePath = System.IO.File.ReadAllText(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "Test"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "Actual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
