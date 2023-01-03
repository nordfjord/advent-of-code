open Printf

module Instr = struct
  type t =
    | AddX of int
    | Noop

  let parse (s : string) =
    match String.split_on_char ' ' s with
    | [ "noop" ] -> Some Noop
    | [ "addx"; x ] -> Some (AddX (int_of_string x))
    | _ -> None

  let toList = function
    | Noop -> [ 0 ]
    | AddX x -> [ 0; x ]
end

let part1 instructions =
  instructions
  |> Seq.concat_map (fun instr -> Instr.toList instr |> List.to_seq)
  |> Seq.scan ( + ) 1
  |> Seq.mapi (fun i x -> (i + 1, x))
  |> Seq.take 221
  |> Seq.filter (fun (i, _) -> i = 20 || (i - 20) mod 40 = 0)
  |> Seq.fold_left (fun sum (i, x) -> sum + (i * x)) 0

let part2 instructions =
  instructions
  |> Seq.concat_map (fun instr -> Instr.toList instr |> List.to_seq)
  |> Seq.scan ( + ) 1
  |> Seq.mapi (fun i spritePosition ->
       let cursorPosition = i mod 40 in
       if abs (spritePosition - cursorPosition) < 2 then '#' else '.')
  |> Seq.iteri (fun i c ->
       if i mod 40 = 0 then printf "\n";
       printf "%c" c)

let instructions = Prelude.Aoc.stdin_seq () |> Seq.filter_map Instr.parse

let () =
  instructions |> part1 |> printf "Part1: %d\n";
  instructions |> part2
