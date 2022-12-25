open Printf

let parse_direction = function
  | '>' -> 1
  | '<' -> -1
  | _ -> failwith "Illegal direction"

(* tuple of width and an array of bitmasks to represent the rock itself *)
let rocks =
  [|
    (4, [ 0b1111 ]);
    (3, [ 0b010; 0b111; 0b010 ]);
    (3, [ 0b111; 0b100; 0b100 ]);
    (1, [ 1; 1; 1; 1 ]);
    (2, [ 0b11; 0b11 ]);
  |]

(* We can represent the state as a list of 7 ints, where each int represents
   the highest point on the map in the column *)

let is_falling rock floor row col =
  let len = Array.length floor in
  rock
  |> List.mapi (fun i rock_row ->
         let idx = row + i in
         if idx < 0 then 127
         else if idx >= len then 0
         else
           let row_value = floor.(idx) in
           row_value land (rock_row lsl col))
  |> List.for_all (fun x -> x = 0)

let adjust_floor height floor rock col row =
  let current_height = Array.length floor in
  if row < 0 then (height, floor)
  else
    let floor =
      Array.append floor
        (Array.make (max 0 (row + List.length rock - current_height)) 0)
    in

    rock
    |> List.iteri (fun i r ->
           floor.(row + i) <- floor.(row + i) lor (r lsl col));

    (* We can trim the array down to only places where it is possible for the next rock to drop
       We do this by scanning the array backwards to find the last visible location *)
    let visible = ref 0 in
    let idx = ref (Array.length floor - 1) in
    while !visible <> 127 && !idx >= 0 do
      let row = floor.(!idx) in
      visible := !visible lor row;
      idx := !idx - 1
    done;
    if !idx = -1 then (height, floor)
    else (!idx + height, Array.sub floor !idx (Array.length floor - !idx))

let print_floor floor =
  for i = Array.length floor - 1 downto 0 do
    printf "|";
    for j = 0 to 6 do
      if floor.(i) land (1 lsl j) <> 0 then printf "#" else printf "."
    done;
    printf "|\n"
  done;
  printf "+-------+\n"

let simulate (width, rock) jets (jet_idx, height, floor) =
  let col = ref 2 in
  let row = ref (Array.length floor + 3) in
  let jet_idx = ref jet_idx in
  let update_col shift =
    let new_col = !col + shift in
    if
      0 <= new_col && new_col <= 7 - width && is_falling rock floor !row new_col
    then col := new_col
  in

  while is_falling rock floor !row !col do
    let jet = jets.[!jet_idx] |> parse_direction in
    jet_idx := (!jet_idx + 1) mod String.length jets;
    update_col jet;
    row := !row - 1
  done;

  row := !row + 1;

  let height, floor = adjust_floor height floor rock !col !row in
  (!jet_idx, height, floor)

let part2_solution rocks_seq jets target =
  let state = ref (0, 0, [||]) in
  let seen = Hashtbl.create 1000 in
  let cycle = ref None in

  rocks_seq
  |> Seq.mapi (fun i rock -> (i, rock))
  |> Seq.take_while (fun (idx, rock) ->
         let jet_idx, height, floor = !state in
         (match Hashtbl.find seen (rock, jet_idx, floor) with
         | last_idx, last_height ->
             let current_height = height in
             cycle :=
               Some
                 ( rock,
                   jet_idx,
                   floor,
                   last_idx,
                   idx - last_idx,
                   last_height,
                   current_height - last_height )
         | exception Not_found ->
             Hashtbl.add seen (rock, jet_idx, floor) (idx, height);
             state := simulate rock jets (jet_idx, height, floor));
         Option.is_none !cycle)
  |> Seq.iter ignore;

  let rock, jet_idx, floor, start_idx, cycle_length, start_height, cycle_height
      =
    Option.get !cycle
  in

  let remaining_rocks = target - start_idx in
  let cycles = remaining_rocks / cycle_length in
  let initial = (jet_idx, start_height + (cycle_height * cycles), floor) in
  let result =
    rocks_seq
    |> Seq.drop_while (fun r -> r <> rock)
    |> Seq.take (target - start_idx - (cycles * cycle_length))
    |> Seq.fold_left (fun state rock -> simulate rock jets state) initial
  in

  result

let input = read_line ()

let part1 () =
  let rocks_seq = rocks |> Array.to_seq |> Seq.cycle in
  let jet_idx, height, floor =
    rocks_seq |> Seq.take 2022
    |> Seq.fold_left (fun state rock -> simulate rock input state) (0, 0, [||])
  in

  printf "%d, %d\n" jet_idx height;
  print_floor floor;
  printf "Part 1: %d\n" (Array.length floor + height)

let part2 () =
  let rock_seq = rocks |> Array.to_seq |> Seq.cycle in
  let jet_idx, height, floor =
    part2_solution rock_seq input 1_000_000_000_000
  in
  printf "%d, %d\n" jet_idx height;
  print_floor floor;
  printf "Part 2: %d\n" (Array.length floor + height)

let () =
  part1 ();
  part2 ()
