let empty =
    Map.ofList (List.init 9 (fun i -> (i, 0L)))

let incrementKey map k =
    map
    |> Map.add k ((Map.tryFind k map |> Option.defaultValue 0L) + 1L)

let parseInput (s: string) =
    s.Split(",")
    |> Array.map int
    |> Array.fold incrementKey empty


let simulateDay (map: Map<int, int64>) =
    map
    |> Map.add 0 (Map.find 1 map)
    |> Map.add 1 (Map.find 2 map)
    |> Map.add 2 (Map.find 3 map)
    |> Map.add 3 (Map.find 4 map)
    |> Map.add 4 (Map.find 5 map)
    |> Map.add 5 (Map.find 6 map)
    |> Map.add 6 ((Map.find 7 map) + (Map.find 0 map))
    |> Map.add 7 (Map.find 8 map)
    |> Map.add 8 (Map.find 0 map)

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
