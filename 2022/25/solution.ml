open Printf

let snafu_char_to_int = function
  | '=' -> -2
  | '-' -> -1
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | _ -> failwith "Illegal SNAFU char"

let int_to_snafu_char = function
  | -2 -> "="
  | -1 -> "-"
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | _ -> failwith "Illegal SNAFU char"

(* OCaml doesn't  have a pow function in the stdlib *)
let rec ( ** ) a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = a ** (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let snafu_to_int str =
  let len = String.length str in
  let result = ref 0 in
  for i = 0 to len - 1 do
    let c = snafu_char_to_int str.[len - i - 1] in
    result := !result + ((5 ** i) * c)
  done;
  !result

let int_to_snafu n =
  let rec loop s = function
    | 0 -> s
    | n ->
        let r = (n + 2) mod 5 in
        let d = (n + 2) / 5 in
        loop (int_to_snafu_char r ^ s) d
  in
  loop "" n

let int_to_snafu value =
  let result = ref value in
  let s = ref "" in
  while !result <> 0 do
    s := int_to_snafu_char (((!result + 2) mod 5) - 2) ^ !s;
    result := (!result - (((!result + 2) mod 5) - 2)) / 5
  done;
  !s

let () =
  printf "Sanity checks\n";
  [ "1121-1110-1=0" ]
  |> List.iter (fun s ->
         printf "%s = %d = %s\n" s (snafu_to_int s)
           (int_to_snafu (snafu_to_int s)))

let lines =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None)
  |> Seq.map snafu_to_int |> Seq.fold_left ( + ) 0 |> int_to_snafu
  |> printf "Part 1: %s\n"
