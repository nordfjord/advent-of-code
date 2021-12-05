let parse lines =
    lines
    |> Seq.map (fun (x: string) ->
        x.ToCharArray()
        |> Array.map (fun x -> int (x.ToString())))
    |> Seq.toArray


let countOccurrences list =
    list
    |> Array.fold
        (fun counts item ->
            counts
            |> Map.change item (fun opt ->
                match opt with
                | Some n -> Some(n + 1)
                | None -> Some 1))
        Map.empty
    |> Map.toList

let toInt l =
    System.Convert.ToInt32(
        l
        |> Seq.map (fun x -> x.ToString())
        |> String.concat (""),
        2
    )


let transpose (matrix: _ [] []) =
    Array.init matrix.[0].Length (fun i -> Array.init matrix.Length (fun j -> matrix.[j].[i]))

let part1 (lines) =
    let transposed = lines |> parse |> transpose

    let gamma =
        transposed
        |> Array.map (
            countOccurrences
            >> List.maxBy (fun (_, value) -> value)
            >> fst
        )
        |> toInt

    let epsilon =
        transposed
        |> Array.map (
            countOccurrences
            >> List.minBy (fun (_, value) -> value)
            >> fst
        )
        |> toInt


    gamma * epsilon

let part2 lines =
    let parsed = lines |> parse

    let rec calculate filter (arr: int [] []) i =
        let len = Array.length arr

        if len = 1 then
            arr.[0]
        else
            let zeroes, ones =
                arr
                |> Array.fold
                    (fun state item ->
                        if item.[i] = 0 then
                            ((fst state) + 1, snd state)
                        else
                            (fst state, (snd state) + 1))
                    (0, 0)


            let filtered =
                arr |> Array.filter (filter zeroes ones i)

            calculate filter filtered (i + 1)

    let o2filter zeroes ones i (digits: int []) =
        (zeroes > ones && digits.[i] = 0)
        || (zeroes <= ones && digits.[i] = 1)

    let co2filter zeroes ones i (digits: int []) =
        (zeroes <= ones && digits.[i] = 0)
        || (ones < zeroes && digits.[i] = 1)

    (calculate o2filter parsed 0 |> toInt)
    * (calculate co2filter parsed 0 |> toInt)


let readLines filePath = System.IO.File.ReadLines(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "Test"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "Actual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
