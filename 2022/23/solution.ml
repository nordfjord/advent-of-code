open Printf

let elves =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None )
  |> Seq.mapi (fun row s ->
         s |> String.to_seq
         |> Seq.mapi (fun col c ->
                match c with '#' -> Some (row, col) | _ -> None ) )
  |> Seq.concat
  |> Seq.filter_map (fun x -> x)
  |> Seq.fold_left
       (fun tbl elf ->
         printf "%d,%d\n" (fst elf) (snd elf) ;
         Hashtbl.add tbl elf true ;
         tbl )
       (Hashtbl.create 300)

type direction = Up | Right | Down | Left

let is_free tbl (row, col) move =
  ( match move with
  | Up ->
      [(row - 1, col); (row - 1, col - 1); (row - 1, col + 1)]
  | Down ->
      [(row + 1, col); (row + 1, col - 1); (row + 1, col + 1)]
  | Left ->
      [(row, col - 1); (row + 1, col - 1); (row - 1, col - 1)]
  | Right ->
      [(row, col + 1); (row + 1, col + 1); (row - 1, col + 1)] )
  |> List.for_all (fun pos -> Hashtbl.mem tbl pos |> not)

let move (row, col) = function
  | Up ->
      (row - 1, col)
  | Down ->
      (row + 1, col)
  | Left ->
      (row, col - 1)
  | Right ->
      (row, col + 1)

let rotate l = match l with [] -> [] | x :: rest -> rest @ [x]

let propose_move tbl dirs pos =
  let results = dirs |> List.filter (is_free tbl pos) in
  match results with
  | [] | [_; _; _; _] ->
      None
  | dir :: _ ->
      Some (pos, move pos dir)

let count f = Array.fold_left (fun sum v -> if f v then sum + 1 else sum) 0

let simulate tbl dirs =
  let proposed_moves =
    tbl |> Hashtbl.to_seq_keys
    |> Seq.filter_map (propose_move tbl dirs)
    |> Array.of_seq
  in
  let unique_moves =
    proposed_moves |> Array.to_seq
    |> Seq.filter (fun (_, dst) ->
           let count = proposed_moves |> count (fun (_, dst2) -> dst = dst2) in
           count = 1 )
  in
  unique_moves
  |> Seq.fold_left
       (fun sum (from_pos, to_pos) ->
         Hashtbl.remove tbl from_pos ;
         Hashtbl.add tbl to_pos true ;
         sum + 1 )
       0

let get_bounds tbl =
  tbl |> Hashtbl.to_seq_keys
  |> Seq.fold_left
       (fun ((r1, c1), (r2, c2)) (r, c) ->
         ((min r1 r, min c1 c), (max r2 r, max c2 c)) )
       ((Int.max_int, Int.max_int), (Int.min_int, Int.min_int))

let print tbl =
  let (min_row, min_col), (max_row, max_col) = get_bounds tbl in
  for row = min_row to max_row do
    for col = min_col to max_col do
      if Hashtbl.mem tbl (row, col) then print_char '#' else print_char '.'
    done ;
    print_char '\n'
  done

let part1 () =
  let dirs = ref [Up; Down; Left; Right] in
  let tbl = Hashtbl.copy elves in
  for i = 1 to 10 do
    print tbl ;
    printf "\nRound %d\n" i ;
    simulate tbl !dirs |> ignore ;
    dirs := rotate !dirs
  done ;
  print tbl ;
  let (min_row, min_col), (max_row, max_col) = get_bounds tbl in
  let width = max_col - min_col + 1 in
  let height = max_row - min_row + 1 in
  let elve_count = Hashtbl.length tbl in
  printf "Part 1: %d\n" ((width * height) - elve_count)

let () = part1 ()

let part2 () =
  let dirs = ref [Up; Down; Left; Right] in
  let tbl = Hashtbl.copy elves in
  let count = ref 1 in
  while simulate tbl !dirs <> 0 do
    count := !count + 1 ;
    dirs := rotate !dirs
  done ;
  printf "Part 2: %d\n" !count

let () = part2 ()
