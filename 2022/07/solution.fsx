open System
open System.Text.RegularExpressions

let readLines = IO.File.ReadAllLines

type Node =
  | File of size: int * name: string
  | Directory of Directory

and Directory =
  { path: string
    parent: Directory option
    children: ResizeArray<Node> }


let initial =
  { path = "/"
    parent = None
    children = ResizeArray() }

let (|ChangeDirectory|_|) (line: string) =
  if line.StartsWith("$ cd") then
    Some(line.Substring 4)
  else
    None

let (|ChangeDirectoryUp|_|) (line: string) =
  if line.StartsWith("$ cd ..") then Some() else None


let (|ListDirectory|_|) (line: string) = if line = "$ ls" then Some() else None

let (|Dir|_|) (line: string) =
  if line.StartsWith("dir ") then
    Some(line.Substring 4)
  else
    None

let (|File|_|) (line: string) =
  let regexp = Regex "([0-9]+) ([a-z]+)"
  let matching = regexp.Match(line)

  if matching.Success then
    let size = int (matching.Groups[1].Value)
    let name = matching.Groups[2].Value
    Some(size, name)
  else
    None


let evolve state (line: string) =
  match line with
  | ChangeDirectoryUp -> state.parent |> Option.get
  | ChangeDirectory path ->
    let dir =
      state.children
      |> Seq.tryPick (function
        | Directory(dir) when dir.path = path -> Some dir
        | _ -> None)

    match dir with
    | Some dir -> dir
    | None ->
      let dir =
        { path = path
          parent = Some state
          children = ResizeArray() }

      state.children.Add(Node.Directory dir)
      dir
  | ListDirectory -> state
  | File(size, name) ->
    let file = Node.File(size, name)

    if not (state.children |> Seq.contains file) then
      state.children.Add(file)

    state
  | Dir _path -> state
  | s -> failwithf "Unexpected %s" s

let rec getRoot tree =
  match tree.parent with
  | Some parent -> getRoot parent
  | None -> tree

let rec calculateSize dir =
  match dir with
  | Node.Directory dir -> Seq.sumBy calculateSize dir.children
  | Node.File(size, _) -> size

let chooseDirectories =
  Seq.choose (function
    | Directory dir -> Some dir
    | _ -> None)

let rec part1 (tree: Directory) =
  let size = tree.children |> Seq.sumBy calculateSize
  let childSize = tree.children |> chooseDirectories |> Seq.sumBy part1
  if size <= 100000 then size + childSize else childSize

let rec findDirectoriesWithEnoughSpace space (tree: Directory) =
  let size = tree.children |> Seq.sumBy calculateSize

  let childSizes =
    chooseDirectories tree.children
    |> Seq.collect (findDirectoriesWithEnoughSpace space)
    |> Seq.toList

  if size >= space then size :: childSizes else childSizes

let part2 (tree: Directory) =
  let requiredSpace = 30000000
  let totalSpace = 70000000
  let usedSpace = calculateSize (Node.Directory tree)
  let unusedSpace = totalSpace - usedSpace
  let spaceToFree = requiredSpace - unusedSpace
  findDirectoriesWithEnoughSpace spaceToFree tree |> List.min

readLines "./input.txt"
|> Array.skip 1
|> Array.fold evolve initial
|> getRoot
|> part1
|> printfn "Part 1: %i"


readLines "./input.txt"
|> Array.skip 1
|> Array.fold evolve initial
|> getRoot
|> part2
|> printfn "Part 2: %A"
