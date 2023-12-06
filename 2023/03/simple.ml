let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

module IntIntSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

let cells = lines.(0) |> String.length
let rows = Array.length lines
let is_digit c = c >= '0' && c <= '9'
let to_digit c = Char.code c - Char.code '0'

let solve arr =
  let num = ref 0 in
  let has_part = ref false in
  let part1 = ref 0 in
  let gears_to_nums = Hashtbl.create 100 in
  for row = 0 to rows do
    num := 0;
    has_part := false;
    let gears = ref IntIntSet.empty in
    for cell = 0 to cells do
      match arr.(row).[cell] with
      | c when is_digit c ->
        num := (!num * 10) + to_digit c;
        for y = row - 1 to row + 1 do
          for x = cell - 1 to cell + 1 do
            if 0 <= x && x < rows && 0 <= y && y < cells
            then (
              let c = arr.(y).[x] in
              if c = '*' then gears := IntIntSet.add (y, x) !gears;
              if c <> '.' && not (is_digit c) then has_part := true)
          done
        done
        (* Invalid_argument is thrown for out of bounds index access *)
      | _ | (exception Invalid_argument _) ->
        part1 := if !has_part then !part1 + !num else !part1;
        if !num > 0
        then
          !gears
          |> IntIntSet.iter (fun (x, y) ->
            let current_gears =
              Hashtbl.find_opt gears_to_nums (x, y) |> Option.value ~default:[]
            in
            Hashtbl.replace gears_to_nums (x, y) (!num :: current_gears));
        gears := IntIntSet.empty;
        num := 0;
        has_part := false
    done
  done;
  let part2 =
    gears_to_nums
    |> Hashtbl.to_seq_values
    |> Seq.fold_left
         (fun acc xs ->
           match xs with
           | [ x; y ] -> acc + (x * y)
           | _ -> acc)
         0
  in
  (!part1, part2)

let () = solve lines |> fun (p1, p2) -> Printf.printf "Part 1: %d\nPart2: %d\n" p1 p2
