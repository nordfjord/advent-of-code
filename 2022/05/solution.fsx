open System.Collections.Generic

let readText = System.IO.File.ReadAllText

let text = readText "./input.txt"

let split (sep: string) (s: string) = s.Split(sep)

let [| towerSetup; instructions |] = text |> split "\n\n"

let setupRows = towerSetup |> split "\n"

let stacks =
  setupRows
  |> Array.map (fun s -> s.ToCharArray())
  |> Array.transpose
  |> Array.map (Array.filter (fun x -> x <> ' ' && x <> ']' && x <> '['))
  |> Array.filter (fun xs -> xs.Length > 0)
  |> Array.map (fun xs -> xs[0 .. xs.Length - 2])
  |> Array.map (fun xs -> Stack(xs))

type Instruction = Move of Count: int * From: int * To: int

let parseInstruction (s: string) =
  if s = "" then
    None
  else
    match split " " s with
    | [| _move; count; _from; from; _to; To |] -> Some(Move(int count, int from - 1, int To - 1))
    | _ -> None

let part1 =
  let stacks = stacks |> Array.map (fun x -> Stack(x)) // clone

  instructions
  |> split "\n"
  |> Array.choose parseInstruction
  |> Array.iter (fun (Move(count, from, To)) ->
    for _ in 1..count do
      match stacks[from].TryPop() with
      | true, v -> stacks[To].Push(v)
      | _ -> ())

  stacks |> Array.map (fun q -> q.Peek().ToString()) |> String.concat ""

let part2 =
  let stacks = stacks |> Array.map (fun x -> Stack(x)) // clone

  instructions
  |> split "\n"
  |> Array.choose parseInstruction
  |> Array.iter (fun (Move(count, from, To)) ->
    seq { 1..count }
    |> Seq.choose (fun _ ->
      match stacks[from].TryPop() with
      | true, x -> Some x
      | _ -> None)
    |> Seq.rev
    |> Seq.iter (fun x -> stacks[To].Push(x)))

  stacks |> Array.map (fun q -> q.Peek().ToString()) |> String.concat ""


printfn "Part 1: %s" part1
printfn "Part 2: %s" part2
