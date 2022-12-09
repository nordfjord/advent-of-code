let readLines = System.IO.File.ReadAllLines

let parseLine (s: string) =
  s.ToCharArray() |> Array.map (string >> int)

let part1 (arr) =
  let (rowMax, colMax) = Array.length arr, Array.length arr[0]

  let isVisible (source: int[][]) row col =
    let treeHeight = source[row][col]

    let hiddenWest =
      seq { 0 .. col - 1 }
      |> Seq.exists (fun i -> arr[row][i] >= treeHeight)

    let hiddenEast =
      seq { col + 1 .. colMax - 1 }
      |> Seq.exists (fun i -> arr[row][i] >= treeHeight)

    let hiddenNorth =
      seq { 0 .. row - 1 }
      |> Seq.exists (fun i -> arr[i][col] >= treeHeight)

    let hiddenSouth =
      seq { row + 1 .. rowMax - 1 }
      |> Seq.exists (fun i -> arr[i][col] >= treeHeight)

    not hiddenWest || not hiddenEast || not hiddenNorth || not hiddenSouth

  [ for i in 0 .. rowMax - 1 do
      for j in 0 .. colMax - 1 do
        if isVisible arr i j then
          1 ]
  |> List.length

module Seq =
  let takeWhileInclusive predicate (source: _ seq) =
    seq {
      use e = source.GetEnumerator()
      let mutable isStopped = false

      while e.MoveNext() && not isStopped do
        yield e.Current

        if not (predicate e.Current) then
          isStopped <- true
    }



let part2 arr =
  let (rowMax, colMax) = Array.length arr, Array.length arr[0]

  let calcScore row col =
    let houseHeight = arr[row][col]

    let west =
      seq { 0 .. col - 1 }
      |> Seq.map (fun i -> arr[row][(col - 1) - i])
      |> Seq.takeWhileInclusive (fun v -> v < houseHeight)
      |> Seq.length

    let east =
      seq { col + 1 .. colMax - 1 }
      |> Seq.map (fun i -> arr[row][i])
      |> Seq.takeWhileInclusive (fun v -> v < houseHeight)
      |> Seq.length

    let north =
      seq { 0 .. row - 1 }
      |> Seq.map (fun i -> arr[(row - 1) - i][col])
      |> Seq.takeWhileInclusive (fun v -> v < houseHeight)
      |> Seq.length

    let south =
      seq { row + 1 .. rowMax - 1 }
      |> Seq.map (fun i -> arr[i][col])
      |> Seq.takeWhileInclusive (fun v -> v < houseHeight)
      |> Seq.length

    east * west * south * north

  [ for row in 1 .. rowMax - 2 do
      for col in 1 .. colMax - 2 do
        calcScore row col ]
  |> List.max

readLines "./input.txt" |> Array.map parseLine |> part1 |> printfn "Part 1: %i"

readLines "./input.txt" |> Array.map parseLine |> part2 |> printfn "Part 2: %i"
