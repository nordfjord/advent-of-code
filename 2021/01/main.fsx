let readLines filePath = System.IO.File.ReadLines(filePath)


type State = { Count: int; Value: int option }

let part1 =
    Seq.fold
        (fun state value ->

            match state.Value with
            | None -> { Count = 0; Value = Some(value) }
            | Some num when value > num ->
                { Count = state.Count + 1
                  Value = Some(value) }
            | Some _ ->
                { Count = state.Count
                  Value = Some(value) })
        { Count = 0; Value = None }


printfn "Test"

readLines "./test.txt"
|> Seq.map int
|> part1
|> printfn "%A"

readLines "./test.txt"
|> Seq.map int
|> Seq.windowed 3
|> Seq.map (Seq.sum)
|> part1
|> printfn "%A"


printfn "Actual"

readLines "./input.txt"
|> Seq.map int
|> part1
|> printfn "%A"

readLines "./input.txt"
|> Seq.map int
|> Seq.windowed 3
|> Seq.map (Seq.sum)
|> part1
|> printfn "%A"
