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

let p1_divisors n = if n % 2 = 0 then Sequence.singleton (n / 2) else Sequence.empty
let divisors n = Sequence.range 1 ((n / 2) + 1) |> Sequence.filter ~f:(fun d -> n % d = 0)

let is_invalid divisors n =
  let nstr = Int.to_string n in
  let digits = String.to_list nstr in
  divisors (String.length nstr)
  |> Sequence.exists ~f:(fun div ->
    List.chunks_of ~length:div digits
    |> List.all_equal ~equal:(List.equal Char.equal)
    |> Option.is_some)

let solve divisors (lo, hi) =
  Sequence.range lo (hi + 1)
  |> Sequence.sum (module Int) ~f:(fun n -> if is_invalid divisors n then n else 0)

let () =
  let rules = lines |> List.hd_exn |> parse in
  rules |> List.sum (module Int) ~f:(solve p1_divisors) |> printf "Part 1: %d\n";
  rules |> List.sum (module Int) ~f:(solve divisors) |> printf "Part 2: %d\n"
