open System
let readText = System.IO.File.ReadAllText

let toChars (s: string) = ReadOnlyMemory(s.ToCharArray())

let isDistinct arr = Set.ofArray arr |> Set.count = arr.Length

let part1 (chars: ReadOnlyMemory<char>) =
  let rec aux (chars: ReadOnlyMemory<char>) idx =
    let subSpan = chars.Slice(0, 4)
    if chars.Length <= 3 then 
      None
    elif isDistinct (subSpan.ToArray()) then
      Some (idx + 4)
    else
      aux (chars.Slice(1)) (idx + 1)

  aux chars 0

let part2 chars =
  let rec aux (chars: ReadOnlyMemory<char>) idx =
    let subSpan = chars.Slice(0, 14)
    if chars.Length <= 13 then 
      None
    elif isDistinct (subSpan.ToArray()) then
      Some (idx + 14)
    else
      aux (chars.Slice(1)) (idx + 1)
  aux chars 0


readText "./input.txt"
|> toChars
|> part1
|> Option.get
|> printfn "Part 1: %i"

readText "./input.txt"
|> toChars
|> part2
|> Option.get
|> printfn "Part 2: %i"

