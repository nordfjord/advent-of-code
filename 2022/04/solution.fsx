let readLines = System.IO.File.ReadAllLines

let parseRange (r: string) =
  let [| from; until |] = r.Split('-')
  seq { int from .. int until } |> Set.ofSeq

let parse (line: string) = 
  let [| a; b |] = line.Split(",")
  parseRange a, parseRange b

let isSuperset (a, b) = 
  a |> Set.isSuperset b || b |> Set.isSuperset a

let hasOverlap (a, b) =
  a |> Set.intersect b |> Set.count > 0



readLines "input.txt"
|> Array.map parse
|> Array.filter isSuperset
|> Array.length
|> printfn "Part 1: %i"

readLines "input.txt"
|> Array.map parse
|> Array.filter hasOverlap 
|> Array.length
|> printfn "Part 2: %i"
