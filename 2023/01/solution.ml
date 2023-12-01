let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

let parse_line str =
  let chars =
    String.to_seq str
    |> Seq.filter (fun c -> '0' <= c && c <= '9')
    |> Seq.map (fun c -> Char.code c - Char.code '0')
    |> Array.of_seq
  in
  if Array.length chars = 0
  then 0
  else (
    let first = chars.(0) in
    let last = chars.(Array.length chars - 1) in
    (first * 10) + last)

let part1 () =
  lines
  |> Array.to_seq
  |> Seq.map parse_line
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n"

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
  ; ("ten", 10)
  ]

let rec parse_line_2 (firstNumber, lastNumber) str =
  if String.length str = 0
  then (
    match (firstNumber, lastNumber) with
    | Some f, Some l -> (f * 10) + l
    | Some f, None -> (f * 10) + f
    | None, Some _ -> failwith "Invalid input"
    | None, None -> failwith "Invalid input")
  else (
    let c = str.[0] in
    let nextStr = String.sub str 1 (String.length str - 1) in
    if '0' <= c && c <= '9'
    then (
      let n = Char.code c - Char.code '0' in
      match (firstNumber, lastNumber) with
      | None, _ -> parse_line_2 (Some n, None) nextStr
      | Some _, _ -> parse_line_2 (firstNumber, Some n) nextStr)
    else (
      let n =
        transformations
        |> List.find_opt (fun (s, _) -> String.starts_with ~prefix:s str)
        |> Option.map snd
      in
      match (n, firstNumber, lastNumber) with
      | None, _, _ -> parse_line_2 (firstNumber, lastNumber) nextStr
      | Some n, None, _ -> parse_line_2 (Some n, None) nextStr
      | Some n, _, _ -> parse_line_2 (firstNumber, Some n) nextStr))

let part2 () =
  lines
  |> Array.to_seq
  |> Seq.map (parse_line_2 (None, None))
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
