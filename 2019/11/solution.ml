open Printf
open Computer

let input = read_line ()

let get_color tbl coord =
  match Hashtbl.find_opt tbl coord with
  | Some x -> x
  | None -> 0

let move p color =
  match p with
  | Halted | Out _ -> failwith "Unexpected state"
  | InputRequested f ->
    (match f color with
     | InputRequested _ | Halted -> failwith "Unexpected state"
     | Out (value1, f) ->
       (match f () with
        | InputRequested _ | Halted -> failwith "Unexpected state"
        | Out (value2, f) -> (f (), (value1, value2))))

let rotate_r = function
  | 1, 0 -> (0, 1)
  | 0, 1 -> (-1, 0)
  | -1, 0 -> (0, -1)
  | 0, -1 -> (1, 0)
  | _ -> failwith "Unexpected coordinates"

let rotate_l = function
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | _ -> failwith "Unexpected coordinates"

let rec simulate floor position direction computer =
  match computer with
  | Halted -> ()
  | Out _ -> failwith "Unexpected out state"
  | InputRequested _ ->
    let next_computer, (paint, rotate) = move computer (get_color floor position) in
    Hashtbl.replace floor position paint;
    let next_direction = if rotate = 0 then rotate_l direction else rotate_r direction in
    let next_position = Prelude.Point.(position + next_direction) in
    simulate floor next_position next_direction next_computer

let part1 () =
  let floor = Hashtbl.create 300 in
  simulate floor (0, 0) (1, 0) (get_program input);
  printf "Part 1: %d\n" (Hashtbl.length floor)

let part2 () =
  let floor = Hashtbl.create 300 in
  Hashtbl.add floor (0, 0) 1;
  simulate floor (0, 0) (1, 0) (get_program input);
  printf "Part 2:\n";
  for i = 1 downto -6 do
    for j = 0 to 40 do
      match Hashtbl.find_opt floor (i, j) with
      | Some 1 -> print_char '#'
      | _ -> print_char '.'
    done;
    print_char '\n'
  done

let () =
  part1 ();
  part2 ()
