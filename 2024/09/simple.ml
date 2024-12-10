open Base
open Stdio

let line = In_channel.input_all stdin
let int_of_char c = Stdlib.int_of_char c - Stdlib.int_of_char '0'

let parse_line line =
  String.to_array line
  |> Array.concat_mapi ~f:(fun i count ->
    let x = if i % 2 = 0 then i / 2 else -1 in
    Array.create ~len:(int_of_char count) x)

let print_disk disk =
  Array.iter disk ~f:(fun x -> if x = -1 then printf "." else printf "%d" x);
  printf "\n"

let part1 disk =
  let disk = Array.copy disk in
  let rec find_empty i = if disk.(i) = -1 then i else find_empty (i + 1) in
  let rec aux empty_idx i =
    let empty_idx = find_empty empty_idx in
    if i < 0 || empty_idx >= i
    then disk
    else (
      disk.(empty_idx) <- disk.(i);
      disk.(i) <- -1;
      aux empty_idx (i - 1))
  in
  aux 0 (Array.length disk - 1)

let count_empty_at disk i =
  let rec aux count i =
    if i >= Array.length disk
    then count
    else if disk.(i) = -1
    then aux (count + 1) (i + 1)
    else count
  in
  aux 0 i

let find_file_range disk x =
  let rec aux start =
    if start < 0
    then (0, x)
    else if disk.(start) = disk.(x)
    then aux (start - 1)
    else (start + 1, x)
  in
  aux x

let part2 disk =
  let disk = Array.copy disk in
  let find_empty size =
    let rec aux i =
      if i >= Array.length disk
      then None
      else if disk.(i) >= 0
      then aux (i + 1)
      else (
        let len = count_empty_at disk i in
        if len >= size then Some i else aux (i + len))
    in
    aux 0
  in
  let rec aux i =
    if i < 0
    then disk
    else if disk.(i) = -1
    then aux (i - 1)
    else (
      let start, end' = find_file_range disk i in
      let len = 1 + (end' - start) in
      match find_empty len with
      | Some empty_start when empty_start < start ->
        Array.fill disk ~pos:empty_start ~len disk.(start);
        Array.fill disk ~pos:start ~len (-1);
        aux (start - 1)
      | _ -> aux (start - 1))
  in
  aux (Array.length disk - 1)

let checksum disk =
  Array.foldi disk ~init:0 ~f:(fun i sum x -> if x = -1 then sum else sum + (i * x))

let () =
  let disk = parse_line line in
  Prelude.Performance.measure "exec" (fun () ->
    part1 disk |> checksum |> printf "Part 1: %d\n";
    part2 disk |> checksum |> printf "Part 2: %d\n")
(* defrag disk part2 |> checksum |> printf "Part 2: %d\n" *)
