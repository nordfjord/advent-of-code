open Base
open Poly
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
  List.equal Int.equal damaged contiguous_damaged

let prepend_to_all x l = Sequence.map ~f:(fun lst -> x :: lst) l

let rec gen = function
  | 0 -> Sequence.singleton []
  | n ->
    let subperms = gen (n - 1) in
    Sequence.append (prepend_to_all '.' subperms) (prepend_to_all '#' subperms)

let permutations springs =
  let arr = springs |> String.to_array in
  let indexes =
    arr
    |> Array.filter_mapi ~f:(fun i c ->
      match c with
      | '?' -> Some i
      | _ -> None)
  in
  gen (Array.length indexes)
  |> Sequence.map ~f:(fun chars ->
    let new_arr = Array.copy arr in
    chars |> List.iteri ~f:(fun i c -> new_arr.(indexes.(i)) <- c);
    new_arr |> String.of_array)

let part1 (springs, damaged) =
  permutations springs |> Sequence.count ~f:(satisfies damaged)

let () = lines |> List.map ~f:parse |> List.sum (module Int) ~f:part1 |> printf "%d\n"

let count_valid_permutations (springs, damaged') =
  let damaged = Array.of_list damaged' in
  let dmglen = Array.length damaged in
  (* Add an extra '.' to the end to ensure our logic below works *)
  let cache = Hashtbl.Poly.create () in
  let rec aux (i, dmg, di) =
    match Hashtbl.find cache (i, dmg, di) with
    | Some x -> x
    | None ->
      if i = String.length springs
      then
        if (di = dmglen && dmg = 0) || (di = dmglen - 1 && damaged.(di) = dmg)
        then 1
        else 0
      else (
        let ans =
          [ '.'; '#' ]
          |> List.fold_left ~init:0 ~f:(fun acc c ->
            if springs.[i] = c || springs.[i] = '?'
            then (
              match (c, dmg) with
              | '.', 0 -> acc + aux (i + 1, 0, di)
              | '.', dmg when dmg > 0 && di < dmglen && damaged.(di) = dmg ->
                acc + aux (i + 1, 0, di + 1)
              | '#', _ -> acc + aux (i + 1, dmg + 1, di)
              | _ -> acc)
            else acc)
        in
        Hashtbl.add cache ~key:(i, dmg, di) ~data:ans |> ignore;
        ans)
  in
  aux (0, 0, 0)

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
