open Base
open Stdio

let towels, desired =
  match In_channel.input_all stdin |> Str.split (Str.regexp_string "\n\n") with
  | [ top; bottom ] -> (Str.split (Str.regexp_string ", ") top, String.split_lines bottom)
  | _ -> failwith "invalid input"

let combinations towel =
  let cache = Hashtbl.create (module Int) in
  let rec aux i =
    if i >= String.length towel
    then 1
    else
      Hashtbl.find_or_add cache i ~default:(fun () ->
        towels
        |> List.filter ~f:(fun x -> String.is_substring_at towel ~pos:i ~substring:x)
        |> List.sum (module Int) ~f:(fun x -> aux (i + String.length x)))
  in
  aux 0

let () =
  let possibilities = List.map desired ~f:combinations in
  List.count possibilities ~f:(fun x -> x > 0) |> printf "Part 1: %d\n";
  List.sum (module Int) possibilities ~f:Fn.id |> printf "Part 2: %d\n"
