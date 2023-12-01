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
  ]

let rec parse_line_2 (firstNumber, lastNumber) i str =
  if i = String.length str
  then (firstNumber * 10) + lastNumber
  else (
    let c = str.[i] in
    match c with
    | c when '0' <= c && c <= '9' ->
      let n = Char.code c - Char.code '0' in
      (match firstNumber with
       | -1 -> parse_line_2 (n, n) (i + 1) str
       | _ -> parse_line_2 (firstNumber, n) (i + 1) str)
    | _ ->
      let remaining_length = String.length str - i in
      let n =
        transformations
        |> List.filter (fun (s, _) -> String.length s <= remaining_length)
        |> List.find_opt (fun (s, _) -> String.sub str i (String.length s) = s)
        |> Option.map snd
      in
      (match n with
       | None -> parse_line_2 (firstNumber, lastNumber) (i + 1) str
       | Some n when firstNumber = -1 -> parse_line_2 (n, n) (i + 1) str
       | Some n -> parse_line_2 (firstNumber, n) (i + 1) str))

let part2 () =
  lines
  |> Array.map (fun l -> parse_line_2 (-1, -1) 0 l)
  |> Array.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
