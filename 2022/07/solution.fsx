open System
open System.Text.RegularExpressions
open System.IO

let readLines = IO.File.ReadAllLines


type State = { cwd: string; fs: Map<string, int> }
let initial = { cwd = "/dev/random"; fs = Map.empty }

let (|ChangeDirectory|_|) (line: string) =
  if line.StartsWith("$ cd ") then
    Some(line.Substring 5)
  else
    None

let (|ListDirectory|_|) (line: string) = if line = "$ ls" then Some() else None

let (|Dir|_|) (line: string) =
  if line.StartsWith("dir ") then
    Some()
  else
    None

let (|File|_|) (line: string) =
  let regexp = Regex "([0-9]+) ([a-z.]+)"
  let matching = regexp.Match(line)

  if matching.Success then
    let size = int (matching.Groups[1].Value)
    let name = matching.Groups[2].Value
    Some(size, name)
  else
    None


let evolve state (line: string) =
  match line with
  | ChangeDirectory path ->
    let newPath = if path.StartsWith "/" then path else Path.GetFullPath(state.cwd + "/" + path) 
    { state with cwd = newPath }
  | ListDirectory -> state
  | File(size, name) ->
    let filePath = Path.Combine(state.cwd, name)
    { state with fs = Map.add filePath size state.fs }
  | Dir -> state
  | s -> failwithf "Unexpected %s" s


let allDirectories (dir: string) =
  dir
    |> List.unfold (function
      | null -> None
      | dir -> Some(dir, Path.GetDirectoryName dir))

let part1 (map: Map<string, int>) =
  let fileList = map |> Map.toList
  fileList
  |> List.collect (fst >> Path.GetDirectoryName >> allDirectories)
  |> Set.ofList
  |> Seq.map (fun dir -> fileList |> List.choose(fun (path, size)-> if path.StartsWith(dir) then Some size else None) |> List.sum)
  |> Seq.filter(fun size -> size <= 100000)
  |> Seq.sum

let rec findDirectoriesWithEnoughSpace space fs =
  let fileList = fs |> Map.toList
  fileList
  |> List.collect (fst >> Path.GetDirectoryName >> allDirectories)
  |> Set.ofList
  |> Seq.map (fun dir -> dir, fileList |> List.choose(fun (path, size)-> if path.StartsWith(dir) then Some size else None) |> List.sum)
  |> Seq.filter (snd >> fun size -> size >= space)
  |> Seq.map snd
  |> Seq.min

let part2 (fs: Map<string, int>) =
  let requiredSpace = 30000000
  let totalSpace = 70000000
  let usedSpace = fs |> Map.toList |> List.sumBy snd
  let unusedSpace = totalSpace - usedSpace
  let spaceToFree = requiredSpace - unusedSpace
  findDirectoriesWithEnoughSpace spaceToFree fs 

readLines "./input.txt"
|> Array.fold evolve initial
|> (fun s -> s.fs)
|> part1
|> printfn "Part 1: %i"


readLines "./input.txt"
|> Array.fold evolve initial
|> (fun s -> s.fs)
|> part2
|> printfn "Part 2: %A"
