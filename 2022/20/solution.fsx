let score (arr: int64 array) =
  let l = arr.Length
  let zi = arr |> Array.findIndex (fun x -> x = 0)
  arr[(zi + 1000) % l] + arr[(zi + 2000) % l] + arr[(zi + 3000) % l]

let switch (arr: (int64 * int) ResizeArray) i item =
  let v, _ = item
  arr.RemoveAt(i)
  let n = int ((int64 i + v) % int64 arr.Count)
  let ni = if n < 0 then arr.Count + n else n
  arr.Insert(ni, item)

let mix original (arr: _ ResizeArray) =
  original
  |> Seq.iter (fun num -> 
    let i = arr.FindIndex(fun x -> x = num)

    switch arr i num)

let shuffle numbers rounds encryptKey =
  let arr = numbers |> Array.mapi (fun i l -> l * encryptKey, i) |> ResizeArray 

  let original = arr.ToArray()

  for _ in 1..rounds do
    mix original arr

  arr.ToArray() |> Array.map (fun (x, _) -> x) |> score


let lines = System.IO.File.ReadAllLines("/dev/stdin")
let numbers = lines |> Array.map int64

let part1 () = shuffle numbers 1 1L
let part2 () = shuffle numbers 10 811589153L

printfn "Part 1: %d" (part1 ())
printfn "Part 2: %d" (part2 ())
