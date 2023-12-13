open Base
open Stdio

let to_mask =
  Array.foldi ~init:0 ~f:(fun i mask c ->
    match c with
    | '#' -> mask lor (1 lsl i)
    | _ -> mask)

let grids =
  In_channel.input_all stdin
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map ~f:(fun s ->
    let lines = String.split_lines s |> List.map ~f:String.to_array |> Array.of_list in
    (Array.map lines ~f:to_mask, Array.transpose_exn lines |> Array.map ~f:to_mask))

module Part1 = struct
  let is_reflected_at arr i =
    let rec aux i j =
      if i < 0 || j >= Array.length arr
      then true
      else if arr.(i) = arr.(j)
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
    match a1 lxor a2 with
    | 0 -> Equal
    | x when x land (x - 1) = 0 -> Smudge
    | _ -> NotEqual

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

let solve find_reflection (rows, cols) =
  match find_reflection rows with
  | Some i -> i * 100
  | None ->
    (match find_reflection cols with
     | Some i -> i
     | None -> failwith "No reflection")

let () =
  let time = Unix.gettimeofday () in
  grids
  |> List.sum (module Int) ~f:(solve (find_reflection Part1.is_reflected_at))
  |> printf "%d\n";
  grids
  |> List.sum (module Int) ~f:(solve (find_reflection Part2.is_reflected_at))
  |> printf "%d\n";
  printf "%fms" ((Unix.gettimeofday() -. time) *. 1000.)
