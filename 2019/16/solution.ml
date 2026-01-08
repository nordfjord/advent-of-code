open Printf

type fft = int array [@@deriving show]

let input =
  read_line ()
  |> String.to_seq
  |> Seq.map (fun c -> Char.code c - Char.code '0')
  |> Array.of_seq

let ( % ) n m = abs n mod m
let base_pattern = [ 0; 1; 0; -1 ]

let create_sequence n =
  base_pattern
  |> List.to_seq
  |> Seq.concat_map (fun x -> Seq.repeat x |> Seq.take n)
  |> Seq.cycle
  |> Seq.drop 1

let fft offset arr =
  arr
  |> Array.mapi (fun i _ ->
    (Seq.zip (Array.to_seq arr) (create_sequence (offset + i + 1))
     |> Seq.map (fun (a, b) -> a * b)
     |> Seq.fold_left ( + ) 0)
    % 10)

let () =
  printf "Part 1: ";
  Seq.ints 0
  |> Seq.take 100
  |> Seq.fold_left (fun inp _ -> fft 0 inp) input
  |> Array.to_seq
  |> Seq.take 8
  |> Seq.iter (fun x -> printf "%d" x);
  printf "\n"

(* Relies on the fact that the offset is always in the back half of the list *)
let phase_backwards_sum digits =
  digits
  |> List.rev
  |> List.fold_left
       (fun (new_list, partial_sum) digit ->
          let new_sum = partial_sum + digit in
          ((new_sum mod 10) :: new_list, new_sum))
       ([], 0)
  |> fst

let solve digits =
  Seq.ints 0
  |> Seq.take 100
  |> Seq.fold_left (fun state _ -> phase_backwards_sum state) digits

let () =
  printf "Part 2: \n";
  let offset =
    Array.to_seq input
    |> Seq.take 7
    |> Seq.fold_left (fun sum digit -> (10 * sum) + digit) 0
  in
  printf "offset=%d\n" offset;
  let input =
    Array.to_seq input
    |> Seq.cycle
    |> Seq.take (Array.length input * 10000)
    |> Seq.drop offset
    |> List.of_seq
  in
  printf "Inp length: %d\n" (List.length input);
  solve input |> List.to_seq |> Seq.take 8 |> Seq.iter (fun x -> printf "%d" x);
  printf "\n"
