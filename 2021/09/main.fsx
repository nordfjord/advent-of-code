type HeightMap = Map<int * int, int>

let parseLine (s: string) = s.Split("") |> Array.map int

let parseMap (input: string seq) =
    [ for (x, line) in input |> Seq.indexed do
          for (y, height) in line |> Seq.indexed -> (x, y), (height |> int) - 48 ]
    |> Map.ofSeq

let neighbors = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

let isLowpoint map (x, y) =
    let value = map |> Map.find (x, y)

    let lowestNeighbor =
        neighbors
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.choose (fun loc -> map |> Map.tryFind loc)
        |> Seq.min

    value < lowestNeighbor

let part1 input =
    let map = parseMap input
    let coords = map |> Map.toSeq

    coords
    |> Seq.map fst
    |> Seq.filter (isLowpoint map)
    |> Seq.map (fun key -> 1 + Map.find key map)
    |> Seq.sum

let calculateBasinSize map lowpoint =
    let mutable visited = Set.empty

    let rec inner point =
        if Set.contains point visited then
            0
        else
            visited <- visited |> Set.add point

            let (x, y) = point

            let neighboringCoords =
                neighbors
                |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
                |> Seq.map (fun p -> p, map |> Map.tryFind p |> Option.defaultValue 9)
                |> Seq.filter (fun (_, height) -> height <> 9)
                |> Seq.map fst


            1 + (neighboringCoords |> Seq.sumBy inner)




    inner lowpoint

let part2 input =
    let map = parseMap input

    let lowPoints =
        map
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.filter (isLowpoint map)

    lowPoints
    |> Seq.map (calculateBasinSize map)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)

let readLines filePath = System.IO.File.ReadLines(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "\nTest"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "\nActual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
