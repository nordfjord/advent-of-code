type Direction =
    | Forward of int
    | Up of int
    | Down of int

let parseDirection (s: string) =
    let split = s.Split(" ")
    let dirString = Array.get split 0
    let amount = Array.get split 1 |> int

    match dirString with
    | "forward" -> Forward amount
    | "up" -> Up amount
    | "down" -> Down amount
    | _ -> failwith "Unexpected direction"

type Coordinates = { x: int; y: int }

let part1 seq =
    seq
    |> Seq.map parseDirection
    |> Seq.fold
        (fun coord direction ->
            match direction with
            | Forward x -> { coord with x = coord.x + x }
            | Up y -> { coord with y = coord.y - y }
            | Down y -> { coord with y = coord.y + y })
        { x = 0; y = 0 }


let multCoord c = c.x * c.y


type AimedCoordinates = { x: int; y: int; aim: int }

let part2 seq =
    seq
    |> Seq.map parseDirection
    |> Seq.fold
        (fun coord direction ->
            match direction with
            | Up x -> { coord with aim = coord.aim - x }
            | Down x -> { coord with aim = coord.aim + x }
            | Forward x ->
                { coord with
                    x = coord.x + x
                    y = coord.y + coord.aim * x })
        { x = 0; y = 0; aim = 0 }

let multAimedCoord c = c.x * c.y

let readLines filePath = System.IO.File.ReadLines(filePath)
printfn "Test"

readLines "./test.txt"
|> part1
|> multCoord
|> printfn "%A"

readLines "./test.txt"
|> part2
|> multAimedCoord
|> printfn "%A"



printfn "Actual"

readLines "./input.txt"
|> part1
|> multCoord
|> printfn "%A"

readLines "./input.txt"
|> part2
|> multAimedCoord
|> printfn "%A"
