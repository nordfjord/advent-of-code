open Base
open Stdio

let calc_b a =
  let b = a % 8 in
  let b = Int.bit_xor b 5 in
  let c = a / Int.pow 2 b in
  let b = Int.bit_xor b c in
  let b = Int.bit_xor b 6 in
  b % 8

let part1 a =
  let rec aux a out =
    let b = calc_b a in
    let a = a / 8 in
    let out = (b % 8) :: out in
    if a = 0 then List.rev out |> List.to_array else aux a out
  in
  aux a []

let expected = [| 2; 4; 1; 5; 7; 5; 4; 3; 1; 6; 0; 3; 5; 5; 3; 0 |]

let print_out out =
  for i = 0 to Array.length out - 1 do
    printf "%d" out.(i);
    if i <> Array.length out - 1 then printf ","
  done;
  printf "\n"

let part2 () =
  let rec solve potential_as i =
    if i < 0
    then List.hd_exn potential_as
    else (
      let next =
        List.fold potential_as ~init:[] ~f:(fun acc a ->
          let a = a * 8 in
          let new_as =
            Sequence.range a (a + 7)
            |> Sequence.filter_map ~f:(fun a ->
              if calc_b a = expected.(i) then Some a else None)
            |> Sequence.to_list
          in
          List.append acc new_as)
      in
      solve next (i - 1))
  in
  solve [ 0 ] 15

let () =
  let p1 = part1 61156655 in
  printf "Part1: ";
  print_out p1;
  part2 () |> printf "Part 2: %d\n"
