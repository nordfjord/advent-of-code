open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse line =
  match String.split line ~on:' ' with
  | [ springs; damaged ] ->
    (springs, String.split damaged ~on:',' |> List.map ~f:Int.of_string)
  | _ -> failwith "invalid input"

let satisfies damaged springs =
  let contiguous_damaged =
    springs
    |> String.to_list_rev
    |> List.fold ~init:[ 0 ] ~f:(fun acc c ->
      match (acc, c) with
      | x :: xs, '#' -> (x + 1) :: xs
      | 0 :: _, '.' -> acc
      | xs, '.' -> 0 :: xs
      | _ -> failwith "invalid spring")
    |> List.filter ~f:(fun x -> x > 0)
  in
  [%equal: int list] damaged contiguous_damaged

let prepend_to_all x l = Sequence.map ~f:(fun lst -> x :: lst) l

let rec gen = function
  | 0 -> Sequence.singleton []
  | n ->
    let subperms = gen (n - 1) in
    Sequence.append (prepend_to_all '.' subperms) (prepend_to_all '#' subperms)

let count_valid_permutations (springs, damaged) =
  let arr = springs |> String.to_array in
  let indexes =
    arr
    |> Array.filter_mapi ~f:(fun i c ->
      match c with
      | '?' -> Some i
      | _ -> None)
  in
  gen (Array.length indexes)
  |> Sequence.count ~f:(fun chars ->
    let new_arr = Array.copy arr in
    chars |> List.iteri ~f:(fun i c -> new_arr.(indexes.(i)) <- c);
    let str = new_arr |> String.of_array in
    satisfies damaged str)

let () =
  lines
  |> List.map ~f:parse
  |> List.sum (module Int) ~f:count_valid_permutations
  |> printf "%d\n"

let memo_rec f =
  let cache = Hashtbl.Poly.create () in
  let rec g x =
    match Hashtbl.find cache x with
    | Some v -> v
    | None ->
      let res = f g x in
      Hashtbl.add cache ~key:x ~data:res |> ignore;
      res
  in
  g

let count_valid_permutations (springs, damaged) =
  let aux aux (i, dmg, dmgs) =
    if i = String.length springs
    then (
      match (dmg, dmgs) with
      | 0, [] -> 1
      | x, [ y ] when x = y -> 1
      | _ -> 0)
    else
      [ '.'; '#' ]
      |> List.fold_left ~init:0 ~f:(fun acc c ->
        if Char.equal springs.[i] c || Char.equal springs.[i] '?'
        then (
          match (c, dmg, dmgs) with
          | '.', 0, _ -> acc + aux (i + 1, 0, dmgs)
          | '.', dmg, x :: xs when dmg = x -> acc + aux (i + 1, 0, xs)
          | '#', _, _ -> acc + aux (i + 1, dmg + 1, dmgs)
          | _ -> acc)
        else acc)
  in
  memo_rec aux (0, 0, damaged)

let () =
  let parse_and_expand line =
    let springs, damaged = parse line in
    ( List.init 5 ~f:(Fn.const springs) |> String.concat ~sep:"?"
    , List.concat (List.init 5 ~f:(Fn.const damaged)) )
  in
  lines
  |> List.map ~f:parse_and_expand
  |> List.sum (module Int) ~f:count_valid_permutations
  |> printf "%d\n"
