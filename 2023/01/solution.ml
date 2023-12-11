open Core

let lines = Stdio.In_channel.input_lines Stdio.In_channel.stdin

let parse_line str =
  let chars =
    str
    |> String.to_array
    |> Array.filter_map ~f:(fun c ->
      match c with
      | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
      | _ -> None)
  in
  if Array.length chars = 0
  then 0
  else (
    let first = chars.(0) in
    let last = chars.(Array.length chars - 1) in
    (first * 10) + last)

let part1 () =
  lines |> List.sum (module Int) ~f:parse_line |> Printf.printf "Part 1: %d\n"

let transformations =
  [ ("one", 1)
  ; ("two", 2)
  ; ("three", 3)
  ; ("four", 4)
  ; ("five", 5)
  ; ("six", 6)
  ; ("seven", 7)
  ; ("eight", 8)
  ; ("nine", 9)
  ]

let rec parse_line_2 (firstNumber, lastNumber) i str =
  if i = String.length str
  then (firstNumber * 10) + lastNumber
  else (
    let c = str.[i] in
    match c with
    | '0' .. '9' ->
      let n = Char.to_int c - Char.to_int '0' in
      (match firstNumber with
       | -1 -> parse_line_2 (n, n) (i + 1) str
       | _ -> parse_line_2 (firstNumber, n) (i + 1) str)
    | _ ->
      let n =
        transformations
        |> List.find_map ~f:(fun (s, n) ->
          if String.is_substring_at str ~pos:i ~substring:s then Some n else None)
      in
      (match n with
       | None -> parse_line_2 (firstNumber, lastNumber) (i + 1) str
       | Some n when firstNumber = -1 -> parse_line_2 (n, n) (i + 1) str
       | Some n -> parse_line_2 (firstNumber, n) (i + 1) str))

let part2 () =
  lines
  |> List.sum (module Int) ~f:(parse_line_2 (-1, -1) 0)
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
