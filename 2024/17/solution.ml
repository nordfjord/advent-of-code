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
    if a = 0 then List.rev out else aux a out
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
    then Sequence.hd_exn potential_as
    else (
      let next =
        Sequence.concat_map potential_as ~f:(fun a ->
          let a = a * 8 in
          Sequence.range a (a + 7)
          |> Sequence.filter ~f:(fun a -> calc_b a = expected.(i)))
      in
      solve next (i - 1))
  in
  solve (Sequence.singleton 0) 15

let () =
  part1 61156655
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:","
  |> printf "Part1: %s\n";
  part2 () |> printf "Part 2: %d\n"
