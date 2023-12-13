open Base
open Stdio

let grids =
  In_channel.input_all stdin
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map ~f:(fun s ->
    s |> String.split_lines |> List.map ~f:String.to_array |> Array.of_list)

let eq = Array.equal Char.equal

let rec extrapolate_reflection arr i j =
  printf "i=%d; j=%d\n" i j;
  if i < 0 || j >= Array.length arr
  then true
  else if eq arr.(i) arr.(j)
  then extrapolate_reflection arr (i - 1) (j + 1)
  else false

let find_reflection arr =
  let rec aux i =
    if i = Array.length arr - 1
    then None
    else if eq arr.(i) arr.(i + 1) && extrapolate_reflection arr i (i + 1)
    then Some i
    else aux (i + 1)
  in
  aux 0

let print grid =
  Array.map grid ~f:String.of_array |> String.concat_array ~sep:"\n" |> print_endline

let solve arr =
  match find_reflection arr with
  | Some i ->
    printf "row %d\n" i;
    (i + 1) * 100
  | None ->
    (match find_reflection (Array.transpose_exn arr) with
     | Some i ->
       printf "col %d\n" i;
       i + 1
     | None ->
       print arr;
       failwith "No reflection")

let () = grids |> List.sum (module Int) ~f:solve |> printf "%d\n"
