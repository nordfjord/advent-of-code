let readLines = System.IO.File.ReadAllLines

let toChars (s: string) = s.ToCharArray()

let parseBag (line: string) =
  let length = line.Length
  let firstCompartment = line.Substring(0, length / 2)
  let secondCompartment = line.Substring(length / 2)
  toChars firstCompartment, toChars secondCompartment

let findError (first, second) =
  Set.ofArray first
  |> Set.intersect (Set.ofArray second)
  |> Seq.head

let a = int 'a'
let A = int 'A'
let scoreChar (c: char) =
  let i = int c
  if i >= a then
    i - a + 1
  else
    i - A + 27

let findBadge (first, second, third) =
  Set.ofArray first
  |> Set.intersect (Set.ofArray second)
  |> Set.intersect (Set.ofArray third)
  |> Seq.head

let toTriplets = function
  | [| a; b; c |] -> Some (a, b, c)
  | _ -> None

readLines "./input.txt"
|> Array.sumBy (parseBag >> findError >> scoreChar)
|> printfn "Part 1: %i"

readLines "./input.txt"
|> Array.map toChars
|> Array.chunkBySize 3
|> Array.choose toTriplets
|> Array.sumBy (findBadge >> scoreChar)
|> printfn "Part 2: %i"
