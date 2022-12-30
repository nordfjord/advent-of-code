open Printf

let input = read_line ()

let range =
  input |> String.split_on_char '-' |> List.map int_of_string
  |> function
  | low :: hi :: _ -> Seq.ints low |> Seq.take (hi - low)
  | _ -> failwith "unexpected input"

let group l =
  match l with
  | [] -> []
  | x :: xs ->
      xs
      |> List.fold_left
           (fun state x ->
             match state with
             | (y :: ys) :: rest when y = x -> (x :: y :: ys) :: rest
             | _ -> [x] :: state )
           [[x]]

let validate password =
  let str = string_of_int password |> String.to_seq |> List.of_seq in
  str |> List.sort compare = str
  && group str |> List.exists (fun g -> List.length g >= 2)

let validate_part2 password =
  let str = string_of_int password |> String.to_seq |> List.of_seq in
  str |> List.sort compare = str
  && group str |> List.exists (fun g -> List.length g = 2)

let () = range |> Seq.filter validate |> Seq.length |> printf "Part 1: %d\n"

let () =
  range |> Seq.filter validate_part2 |> Seq.length |> printf "Part 2: %d\n"
