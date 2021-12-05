type Point = int * int
type Line = Point * Point

let parsePoint (s: string) =
    let ([| x; y |]) = s.Split(",")
    (int x, int y)


let parseLine (s: string) =
    let ([| p1; p2 |]) = s.Split(" -> ")
    (parsePoint p1, parsePoint p2)

let pointsOfLine ((x1, y1), (x2, y2)) =
    let mutable points = List.empty
    let fromX = min x1 x2
    let toX = max x1 x2
    let fromY = min y1 y2
    let toY = max y1 y2

    if fromX = toX || fromY = toY then
        for x in fromX .. toX do
            for y in fromY .. toY do
                points <- (x, y) :: points
    else
        let lineLength = toY - fromY
        let xOp = if x1 < x2 then (+) else (-)
        let yOp = if y1 < y2 then (+) else (-)

        for i in 0 .. lineLength do
            points <- (xOp x1 i, yOp y1 i) :: points

    points


let isHorizontalOrVertical l =
    let ((x1, y1), (x2, y2)) = l
    x1 = x2 || y1 = y2

let concatPointMaps m1 m2 =
    m1
    |> Map.toList
    |> List.fold
        (fun map (k, v) ->
            map
            |> Map.change k (fun x ->
                match x with
                | Some x -> Some(x + v)
                | None -> Some v))
        m2

let addPointToMap map point =
    map
    |> Map.change point (function
        | Some v -> Some(v + 1)
        | None -> Some 1)

let part1 input =
    input
    |> Seq.map parseLine
    |> Seq.filter isHorizontalOrVertical
    |> Seq.toList
    |> List.collect pointsOfLine
    |> (List.fold addPointToMap Map.empty)
    |> Map.toList
    |> List.filter (fun (k, v) -> v > 1)
    |> List.length

let part2 input =
    input
    |> Seq.map parseLine
    |> Seq.toList
    |> List.collect pointsOfLine
    |> (List.fold addPointToMap Map.empty)
    |> Map.toList
    |> List.filter (fun (_, v) -> v > 1)
    |> List.length



let readLines filePath = System.IO.File.ReadLines(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "Test"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "Actual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
