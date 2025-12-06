open Base
open Stdio

let lines = In_channel.input_all stdin

let ranges, ids =
  match Str.split (Str.regexp_string "\n\n") lines with
  | [ a; b ] -> (a, b)
  | _ -> failwith "too many newlines"

let ranges =
  ranges
  |> String.split_lines
  |> List.map ~f:(fun s ->
    String.split s ~on:'-'
    |> List.map ~f:Int.of_string
    |> function
    | [ a; b ] -> (a, b)
    | _ -> failwith "wtf")

let ids = ids |> String.split_lines |> List.map ~f:Int.of_string

module Range = struct
  type t = int * int [@@deriving compare, sexp, equal, hash]

  let length (lo, hi) = hi - lo + 1
  let contains n (lo, hi) = lo <= n && n <= hi

  let dedupe ranges =
    List.sort ranges ~compare
    |> List.fold ~init:[] ~f:(fun acc r ->
      match acc with
      | [] -> [ r ]
      | (prev_lo, prev_hi) :: rest ->
        let lo, hi = r in
        if lo <= prev_hi (* overlapping or adjacent *)
        then (prev_lo, max prev_hi hi) :: rest
        else r :: acc)
end

let is_fresh n = List.exists ranges ~f:(Range.contains n)
let part1 () = List.count ids ~f:is_fresh
let part2 () = Range.dedupe ranges |> List.sum (module Int) ~f:Range.length

let () =
  part1 () |> printf "Part 1: %d\n";
  part2 () |> printf "Part 2: %d\n"
