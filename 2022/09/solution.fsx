type Coordinates = int * int
type Movement = Up | Down | Left | Right

let update move (x, y) =
  match move with
  | Up -> x, y + 1
  | Down -> x, y - 1
  | Left -> x - 1, y
  | Right -> x + 1, y

let updateTail (headX, headY) (x, y) =
  let dx = headX - x
  let dy = headY - y
  if abs dx <= 1 && abs dy <= 1 then
    x, y
  elif abs dx > 1 && dy = 0 then
    let step = dx / abs dx
    x + step, y
  elif abs dy > 1 && dx = 0 then
    let step = dy / abs dy
    x, y + step
  else
    let stepX = dx / abs dx
    let stepY = dy / abs dy
    x + stepX, y + stepY

module Part1 =
  type State = { Head: Coordinates; Tail: Coordinates; Visited: Set<Coordinates> }

  let initial = { Head = 0,0; Tail = 0,0; Visited = Set.singleton (0,0)}
  let evolve state move =
    let head = update  move state.Head
    let tail = updateTail head state.Tail

    { Head = head; Tail = tail; Visited = state.Visited |> Set.add tail }

module Part2 =
  type State = { Knots: Coordinates list; Visited: Set<Coordinates> }
  let initial = { Knots = List.init 10 (fun _ -> 0,0); Visited = Set.singleton (0,0) }
  let evolve state move =
    let head = List.head state.Knots |> update move
    let knots =
      (head ::
      (state.Knots
      |> List.skip 1))
      |> List.pairwise
      |> List.map (fun (head, tail) -> updateTail head tail)
    { Knots = head::knots; Visited = Set.add (List.last knots) state.Visited }


let parseLine (s: string) =
  match s.Split(" ") with
  | [| "R"; count |] -> Array.create (int count) Right |> Some
  | [| "L"; count |] -> Array.create (int count) Left |> Some
  | [| "U"; count |] -> Array.create (int count) Up |> Some
  | [| "D"; count |] -> Array.create (int count) Down |> Some
  | _ -> None


let readLines = System.IO.File.ReadAllLines

readLines "./input.txt"
  |> Seq.choose parseLine
  |> Seq.collect id
  |> Seq.fold Part1.evolve Part1.initial
  |> (fun s -> s.Visited |> Set.count)
  |> printfn "Part 1: %A"


readLines "./input.txt"
  |> Seq.choose parseLine
  |> Seq.collect id
  |> Seq.fold Part2.evolve Part2.initial
  |> (fun s -> s.Visited |> Set.count)
  |> printfn "Part 1: %A"
