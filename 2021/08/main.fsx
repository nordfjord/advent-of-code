let decode (s: string) =
    let [| wires; digits |] = s.Split(" | ")
    (wires.Split(" ") |> Array.map Set.ofSeq, digits.Split(" ") |> Array.map Set.ofSeq)

let part1 lines =
    lines
    |> Seq.map decode
    |> Seq.map snd
    |> Seq.collect (
        Seq.filter (fun s ->
            let len = s.Count
            len = 2 || len = 4 || len = 3 || len = 7)
    )
    |> Seq.length


let mappings =
    Map.ofList [ ("abcefg", 0)
                 ("cf", 1)
                 ("acdeg", 2)
                 ("acdfg", 3)
                 ("bcdf", 4)
                 ("abdfg", 5)
                 ("abdefg", 6)
                 ("acf", 7)
                 ("abcdefg", 8)
                 ("abcdfg", 9) ]

let inferOutput (patterns, output) =
    let ofLength n segments =
        segments
        |> Seq.filter (fun s -> s |> Seq.length = n)

    // cf
    let one = patterns |> ofLength 2 |> Seq.head
    // bcdf
    let four = patterns |> ofLength 4 |> Seq.head
    // acf
    let seven = patterns |> ofLength 3 |> Seq.head
    // abcdefg
    let eight = patterns |> ofLength 7 |> Seq.head

    let cf = one
    let bd = Set.difference four cf

    let fiveSegments = patterns |> ofLength 5

    let three =
        fiveSegments |> Seq.find (Set.isSubset cf)

    let five =
        fiveSegments |> Seq.find (Set.isSubset bd)

    let two =
        fiveSegments
        |> Seq.filter (fun x -> x <> three && x <> five)
        |> Seq.head

    let sixSegments = patterns |> ofLength 6

    let six =
        sixSegments |> Seq.find (Set.isSubset cf >> not)

    let nine =
        sixSegments
        |> Seq.filter (Set.isSubset cf)
        |> Seq.filter (Set.isSubset bd)
        |> Seq.head

    let zero =
        sixSegments
        |> Seq.filter (fun x -> x <> six && x <> nine)
        |> Seq.head

    let table =
        Map.ofSeq [ zero, 0
                    one, 1
                    two, 2
                    three, 3
                    four, 4
                    five, 5
                    six, 6
                    seven, 7
                    eight, 8
                    nine, 9 ]

    output
    |> Seq.map (fun key ->
        try
            Map.find key table
        with
        | _ ->
            printfn "%A" key
            0)
    |> Seq.map string
    |> String.concat ""
    |> int





let part2 lines =
    lines
    |> Seq.map decode
    |> Seq.map inferOutput
    |> Seq.sum

let readLines filePath = System.IO.File.ReadLines(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "\nTest"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "\nSimple test"

readLines "./simple-test.txt"
|> part2
|> printfn "Part2: %A"


printfn "\nActual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
