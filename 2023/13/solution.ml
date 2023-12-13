open Base
open Stdio

let grids =
  In_channel.input_all stdin
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map ~f:(fun s ->
    s |> String.split_lines |> List.map ~f:String.to_array |> Array.of_list)

module Part1 = struct
  let eq = Array.equal Char.equal

  let is_reflected_at arr i =
    let rec aux i j =
      if i < 0 || j >= Array.length arr
      then true
      else if eq arr.(i) arr.(j)
      then aux (i - 1) (j + 1)
      else false
    in
    if i = Array.length arr - 1 then false else aux i (i + 1)
end

module Part2 = struct
  type eq =
    | Smudge
    | Equal
    | NotEqual

  let smudge_eq a1 a2 =
    let rec aux count i =
      if count > 1
      then NotEqual
      else if i = Array.length a1
      then if count = 1 then Smudge else Equal
      else if Char.equal a1.(i) a2.(i)
      then aux count (i + 1)
      else aux (count + 1) (i + 1)
    in
    aux 0 0

  let is_reflected_at arr i =
    let rec aux i j smudges =
      if smudges > 1
      then false
      else if i < 0 || j >= Array.length arr
      then if smudges = 1 then true else false
      else (
        match smudge_eq arr.(i) arr.(j) with
        | Smudge -> aux (i - 1) (j + 1) (smudges + 1)
        | Equal -> aux (i - 1) (j + 1) smudges
        | NotEqual -> false)
    in
    aux i (i + 1) 0
end

let find_reflection is_reflected_at arr =
  let rec aux i =
    if i = Array.length arr
    then None
    else if is_reflected_at arr i
    then Some (i + 1)
    else aux (i + 1)
  in
  aux 0

let solve find_reflection arr =
  match find_reflection arr with
  | Some i -> i * 100
  | None ->
    (match find_reflection (Array.transpose_exn arr) with
     | Some i -> i
     | None -> failwith "No reflection")

let () =
  grids
  |> List.sum (module Int) ~f:(solve (find_reflection Part1.is_reflected_at))
  |> printf "%d\n";
  grids
  |> List.sum (module Int) ~f:(solve (find_reflection Part2.is_reflected_at))
  |> printf "%d\n"
