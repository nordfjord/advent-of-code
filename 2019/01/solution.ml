open Stdio

let calc m = (m / 3) - 2

let rec calc2 m =
  let next = calc m in
  if next <= 0 then 0 else next + calc2 next

let ( >> ) f g x = g (f x)

let () =
  let lines = In_channel.input_lines stdin in
  lines
  |> List.map (int_of_string >> calc)
  |> List.fold_left ( + ) 0
  |> printf "Part 1: %d\n";
  lines
  |> List.map (int_of_string >> calc2)
  |> List.fold_left ( + ) 0
  |> printf "Part 2: %d\n"
