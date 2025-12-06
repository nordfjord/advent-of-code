open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse str =
  String.to_array str |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0')

let bits_to_int bits =
  Array.foldi bits ~init:0 ~f:(fun i acc bit ->
    acc + (bit lsl (Array.length bits - i - 1)))

let part1 numbers =
  let nsize = Array.length numbers.(0) in
  let rec aux (gamma, epsilon) i =
    if i >= Array.length numbers.(0)
    then (gamma, epsilon)
    else (
      let ones, zeroes =
        Array.fold numbers ~init:(0, 0) ~f:(fun (ones, zeros) number ->
          if number.(i) = 1 then (ones + 1, zeros) else (ones, zeros + 1))
      in
      if ones > zeroes
      then aux (gamma lor (1 lsl (nsize - i - 1)), epsilon) (i + 1)
      else aux (gamma, epsilon lor (1 lsl (nsize - i - 1))) (i + 1))
  in
  let gamma, epsilon = aux (0, 0) 0 in
  gamma * epsilon

let part2 numbers =
  let rec calculate filter arr i =
    if Array.length arr = 1
    then arr.(0)
    else (
      let zeroes, ones =
        Array.fold
          arr
          ~f:(fun (zeroes, ones) item ->
            if item.(i) = 0 then (zeroes + 1, ones) else (zeroes, ones + 1))
          ~init:(0, 0)
      in
      let filtered = Array.filter arr ~f:(filter zeroes ones i) in
      calculate filter filtered (i + 1))
  in
  let most_common zeroes ones i digits =
    (zeroes > ones && digits.(i) = 0) || (zeroes <= ones && digits.(i) = 1)
  in
  let least_common zeroes ones i digits = not @@ most_common zeroes ones i digits in
  (calculate most_common numbers 0 |> bits_to_int)
  * (calculate least_common numbers 0 |> bits_to_int)

let () =
  let numbers = lines |> List.map ~f:parse |> List.to_array in
  part1 numbers |> printf "Part 1: %d\n";
  part2 numbers |> printf "Part 2: %d\n"
