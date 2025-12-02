open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse str =
  String.split ~on:',' str
  |> List.map ~f:(fun x ->
    let parts = String.split ~on:'-' x in
    match parts with
    | [ a; b ] -> (Int.of_string a, Int.of_string b)
    | _ -> failwith "Invalid input format")

let p1_divisors n = if n % 2 = 0 then [ n / 2 ] else []

let divisors n =
  let rec aux d acc =
    if d > n / 2
    then acc
    else if n % d = 0
    then aux (d + 1) (d :: acc)
    else aux (d + 1) acc
  in
  aux 1 []

let solve divisors (lo, hi) =
  let rec aux n acc =
    if n > hi
    then acc
    else (
      let nstr = Int.to_string n in
      let digits = String.to_list nstr in
      let divs = divisors (String.length nstr) in
      let has_invalid =
        divs
        |> List.exists ~f:(fun div ->
          List.chunks_of ~length:div digits
          |> List.all_equal ~equal:(List.equal Char.equal)
          |> Option.is_some)
      in
      match has_invalid with
      | true -> aux (n + 1) (n + acc)
      | false -> aux (n + 1) acc)
  in
  aux lo 0

let () =
  let rules = lines |> List.hd_exn |> parse in
  rules |> List.sum (module Int) ~f:(solve p1_divisors) |> printf "Part 1: %d\n";
  rules |> List.sum (module Int) ~f:(solve divisors) |> printf "Part 2: %d\n"
