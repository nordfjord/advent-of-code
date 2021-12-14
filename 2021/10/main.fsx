let split (s: string) = s.ToCharArray()

type ParseState = { Pos: int; Stack: string list }

let score (c: char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let rec findIllegalCharacter s =
    let rec parse (line: char []) pos stack =
        let expectedTag = List.tryHead stack

        match line |> Array.tryItem pos with
        | None -> None
        | Some '(' -> parse line (pos + 1) (')' :: stack)
        | Some '[' -> parse line (pos + 1) (']' :: stack)
        | Some '{' -> parse line (pos + 1) ('}' :: stack)
        | Some '<' -> parse line (pos + 1) ('>' :: stack)
        | _ ->
            match expectedTag with
            | None -> Some line.[pos]
            | Some v ->
                if v = line.[pos] then
                    parse line (pos + 1) (List.tail stack)
                else
                    Some line.[pos]

    parse s 0 []

let part1 input =
    input
    |> Seq.map (split)
    |> Seq.choose findIllegalCharacter
    |> Seq.sumBy score

let rec parseAndFix s =
    let rec parse (line: char []) pos stack =
        let expectedTag = List.tryHead stack

        match line |> Array.tryItem pos with
        | None -> stack |> List.toArray
        | Some '(' -> parse line (pos + 1) (')' :: stack)
        | Some '[' -> parse line (pos + 1) (']' :: stack)
        | Some '{' -> parse line (pos + 1) ('}' :: stack)
        | Some '<' -> parse line (pos + 1) ('>' :: stack)
        | _ ->
            match expectedTag with
            | None -> parse line (pos + 1) stack
            | Some v ->
                if v = line.[pos] then
                    parse line (pos + 1) (List.tail stack)
                else
                    parse line (pos + 1) stack

    parse s 0 []

let round2Score (c: char []) =
    let score c =
        match c with
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> 0L

    let result =
        c
        |> Array.fold (fun total curr -> (total * 5L) + (score curr)) 0L

    if result < 0L then printfn "%A" c
    result

let part2 input =
    let results =
        input
        |> Seq.map split
        |> Seq.filter (findIllegalCharacter >> Option.isNone)
        |> Seq.map parseAndFix
        |> Seq.map (round2Score)
        |> Seq.sort
        |> Seq.toArray

    results.[results.Length / 2]


let readLines filePath = System.IO.File.ReadLines(filePath)

let testLines = readLines "./test.txt"
let inputLines = readLines "./input.txt"


printfn "\nTest"
testLines |> part1 |> printfn "Part1: %A"
testLines |> part2 |> printfn "Part2: %A"


printfn "\nActual"

inputLines |> part1 |> printfn "Part1: %A"
inputLines |> part2 |> printfn "Part2: %A"
