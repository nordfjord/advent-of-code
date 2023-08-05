let input = read_line () |> String.split_on_char ',' |> List.map int_of_string

let simulate days fishes =
  let fish = Array.make 9 0 in
  fishes |> List.iter (fun f -> fish.(f) <- fish.(f) + 1);
  for _ = 1 to days do
    let zeroes = fish.(0) in
    fish.(0) <- fish.(1);
    fish.(1) <- fish.(2);
    fish.(2) <- fish.(3);
    fish.(3) <- fish.(4);
    fish.(4) <- fish.(5);
    fish.(5) <- fish.(6);
    fish.(6) <- fish.(7) + zeroes;
    fish.(7) <- fish.(8);
    fish.(8) <- zeroes
  done;
  fish |> Array.fold_left ( + ) 0

let tick = function
  | 0 -> [ 6; 8 ]
  | n -> [ n - 1 ]

let rec simulate_naive days fishes =
  if days = 0
  then List.length fishes
  else simulate_naive (days - 1) (List.concat_map tick fishes)

let () =
  input |> simulate 80 |> Printf.printf "Part 1: %d\n";
  input |> simulate 256 |> Printf.printf "Part 2: %d\n";
  input |> simulate_naive 80 |> Printf.printf "Part 1: %d\n%!";
  input |> simulate_naive 256 |> Printf.printf "Part 2: %d\n"
