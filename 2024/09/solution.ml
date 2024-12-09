open Base
open Stdio

let line = In_channel.input_all stdin

type file =
  { id : int
  ; size : int
  }
[@@deriving sexp, equal]

type block =
  | Empty of int
  | File of file
[@@deriving sexp, equal]

type block_array = block array [@@deriving sexp]

let int_of_char c = Stdlib.int_of_char c - Stdlib.int_of_char '0'

let parse_line line =
  let rec aux i acc =
    if i >= String.length line
    then acc
    else if i % 2 = 0
    then (
      let size = int_of_char line.[i] in
      aux (i + 1) (File { id = i / 2; size } :: acc))
    else (
      let size = int_of_char line.[i] in
      aux (i + 1) (Empty size :: acc))
  in
  aux 0 [] |> List.rev

let rec part1 disk file =
  match disk with
  | [] -> []
  | File f :: _ when equal_file f file -> disk
  | File { id; size } :: rest when file.size = 0 && id = file.id -> Empty size :: rest
  | File { id; size } :: rest when id = file.id -> File file :: Empty size :: rest
  | Empty size :: rest when file.size > 0 && size >= file.size ->
    File file :: Empty (size - file.size) :: part1 rest { file with size = 0 }
  | Empty size :: rest when file.size > 0 ->
    File { file with size } :: part1 rest { file with size = file.size - size }
  | x :: xs -> x :: part1 xs file

let part2 disk file =
  let rec aux inserted disk file =
    match disk with
    | [] -> []
    | File { id; _ } :: rest when inserted && id = file.id -> Empty file.size :: rest
    | File { id; _ } :: _ when id = file.id -> disk
    | Empty size :: rest when (not inserted) && size = file.size ->
      File file :: aux true rest file
    | Empty size :: rest when (not inserted) && size > file.size ->
      File file :: Empty (size - file.size) :: aux true rest file
    | x :: xs -> x :: aux inserted xs file
  in
  aux false disk file

let defrag disk move_file =
  let files =
    List.filter_map disk ~f:(function
      | File x -> Some x
      | _ -> None)
    |> List.rev
  in
  List.fold files ~init:disk ~f:move_file

let checksum disk =
  let rec file_sum loc { id; size } =
    if size = 0 then 0 else (loc * id) + file_sum (loc + 1) { id; size = size - 1 }
  in
  let rec aux disk loc sum =
    match disk with
    | [] -> sum
    | File file :: rest -> aux rest (loc + file.size) (sum + file_sum loc file)
    | Empty size :: rest -> aux rest (loc + size) sum
  in
  aux disk 0 0

let () =
  let disk = parse_line line in
  defrag disk part1 |> checksum |> printf "Part 1: %d\n";
  defrag disk part2 |> checksum |> printf "Part 2: %d\n"
