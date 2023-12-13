open Base
open Stdio

let grids =
  In_channel.input_all stdin
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map ~f:(fun s ->
    s |> String.split_lines |> List.map ~f:String.to_array |> Array.of_list)

let smudge_eq a1 a2 =
  let rec aux count i =
    if count > 1
    then `not_eq
    else if i = Array.length a1
    then if count = 1 then `smudge else `eq
    else if Char.equal a1.(i) a2.(i)
    then aux count (i + 1)
    else aux (count + 1) (i + 1)
  in
  aux 0 0

let validate_reflection arr i =
  let rec aux i j smudges =
    let out_of_bounds = i < 0 || j >= Array.length arr in
    if smudges > 1
    then false
    else if out_of_bounds
    then if smudges = 1 then true else false
    else (
      match smudge_eq arr.(i) arr.(j) with
      | `smudge -> aux (i - 1) (j + 1) (smudges + 1)
      | `eq -> aux (i - 1) (j + 1) smudges
      | `not_eq -> false)
  in
  aux i (i + 1) 0

let find_reflection arr =
  let rec aux i =
    if i = Array.length arr - 1
    then None
    else if validate_reflection arr i
    then Some i
    else aux (i + 1)
  in
  aux 0

let print grid =
  Array.mapi grid ~f:(fun i s -> Printf.sprintf "%2d %s" i (String.of_array s))
  |> String.concat_array ~sep:"\n"
  |> print_endline

let solve arr =
  match find_reflection arr with
  | Some i -> (i + 1) * 100
  | None ->
    (match find_reflection (Array.transpose_exn arr) with
     | Some i -> i + 1
     | None ->
       print arr;
       printf "\n";
       print (Array.transpose_exn arr);
       failwith "No reflection")

let () = grids |> List.sum (module Int) ~f:solve |> printf "%d\n"
