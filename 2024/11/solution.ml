open Base
open Stdio

let set_or_add x = function
  | None -> x
  | Some y -> x + y

let line =
  In_channel.input_all stdin
  |> String.split ~on:' '
  |> List.fold
       ~init:(Hashtbl.create (module Int))
       ~f:(fun map x ->
         Hashtbl.update map (Int.of_string x) ~f:(set_or_add 1);
         map)

let rec num_digits n = if n = 0 then 0 else 1 + num_digits (n / 10)

let split_number n =
  let half = num_digits n / 2 in
  let divider = 10 ** half in
  [ n / divider; n % divider ]

let transform = function
  | 0 -> [ 1 ]
  | n when num_digits n % 2 = 0 -> split_number n
  | n -> [ n * 2024 ]

let blink map =
  let next = Hashtbl.create (module Int) in
  Hashtbl.iteri map ~f:(fun ~key ~data:count ->
    transform key |> List.iter ~f:(Hashtbl.update next ~f:(set_or_add count)));
  next

let rec simulate map n =
  if n = 0
  then Hashtbl.fold map ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data)
  else simulate (blink map) (n - 1)

type intlist = int list [@@deriving sexp]

let () =
  simulate line 25 |> printf "Part 1: %d\n";
  simulate line 75 |> printf "Part 2: %d\n"
