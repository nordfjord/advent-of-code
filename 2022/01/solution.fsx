let readLines filePath = System.IO.File.ReadAllText(filePath)

let split (separator: string) (value: string) = value.Split(separator)

readLines "./input.txt"
|> split "\n\n"
|> Seq.map (split "\n" >> Seq.map int >> Seq.sum)
|> Seq.max
|> printfn "Solution 1: %i"

readLines "./input.txt"
|> split "\n\n"
|> Seq.map (split "\n" >> Seq.map int >> Seq.sum)
|> Seq.sortDescending
|> Seq.take 3
|> Seq.sum
|> printfn "Solution 2: %i"
