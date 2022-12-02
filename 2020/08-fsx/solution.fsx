let readLines = System.IO.File.ReadAllLines

type Instruction =
  | Acc of int
  | Nop of int
  | Jmp of int

type State =
  { Acc: int
    InstrPtr: int
    Program: Instruction[] 
    Visited: bool[] }

let initialState program =
  { Acc = 0; InstrPtr = 0; Program = program; Visited = Array.create program.Length false }

let rec interpret state =
  if state.Visited[state.InstrPtr] then
    state.Acc
  else
    state.Visited[state.InstrPtr] <- true
    match state.Program[state.InstrPtr] with
    | Nop _ -> interpret { state with InstrPtr = state.InstrPtr + 1 }
    | Jmp v -> interpret { state with InstrPtr = state.InstrPtr + v }
    | Acc v -> interpret { state with InstrPtr = state.InstrPtr + 1; Acc = state.Acc + v}

let rec interpretWithTerminality state =
  if state.InstrPtr >= state.Visited.Length - 1 then
    Some state.Acc
  elif state.Visited[state.InstrPtr] then
    None
  else
    state.Visited[state.InstrPtr] <- true
    match state.Program[state.InstrPtr] with
    | Nop _ -> interpretWithTerminality { state with InstrPtr = state.InstrPtr + 1 }
    | Acc v -> interpretWithTerminality { state with Acc = state.Acc + v; InstrPtr = state.InstrPtr + 1 }
    | Jmp v -> interpretWithTerminality { state with InstrPtr = state.InstrPtr + v }


module Array =
  let set idx v (arr: _ array) = arr[idx] <- v; arr
  let clone (arr: 'a array) : 'a array = unbox (arr.Clone())

let findCorrectProgram state =
  let programs =
    Array.init state.Program.Length (fun idx -> 
      match state.Program[idx] with
      | Acc _ -> None
      | Jmp v -> Some { state with 
                          Program = state.Program |> Array.clone |> Array.set idx (Nop v)
                          Visited = Array.clone state.Visited }
      | Nop v -> Some { state with 
                          Program = state.Program |> Array.clone |> Array.set idx (Jmp v)
                          Visited = Array.clone state.Visited })
  programs |> Array.choose id |> Array.choose interpretWithTerminality |> Array.head


let parseLine (s: string) =
  match s.Split(" ") with
  | [| t; v |] ->
    match t with
    | "nop" -> Nop (int v) |> Some
    | "jmp" -> Jmp (int v) |> Some
    | "acc" -> Acc (int v) |> Some
    | _ -> None
  | _ -> None


readLines "./input.txt"
|> Array.choose parseLine
|> initialState
|> interpret
|> printfn "Part1: %i"


readLines "./input.txt"
|> Array.choose parseLine
|> initialState
|> findCorrectProgram
|> printfn "Part2: %i"

