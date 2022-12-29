open Printf

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with x -> Some x | exception End_of_file -> None )

type block = Air | Rock | Sand

type paths = block array array

let rec pairwise l =
  match l with
  | [] ->
      []
  | x :: y :: xs ->
      (x, y) :: pairwise (y :: xs)
  | _ :: [] ->
      []

let lines =
  lines
  |> Seq.map (fun line ->
         line
         |> Str.split (Str.regexp_string " -> ")
         |> List.map (fun segment ->
                Scanf.sscanf segment "%d,%d" (fun col row -> (col, row)) ) )
  |> List.of_seq

let max_col, max_row =
  lines |> List.flatten
  |> List.fold_left
       (fun (max_col, max_row) (col, row) -> (max col max_col, max row max_row))
       (0, 0)

let max_col, max_row = (max_col + 1000, max_row + 1)

let initial : paths =
  Array.init max_row (fun _ -> Array.init max_col (fun _ -> Air))

let () =
  lines
  |> List.iter (fun line ->
         line |> pairwise
         |> List.iter (fun ((col1, row1), (col2, row2)) ->
                for col = min col1 col2 to max col1 col2 do
                  for row = min row1 row2 to max row1 row2 do
                    initial.(row).(col) <- Rock
                  done
                done ) )

let cave = Array.map Array.copy initial

let simulate cave =
  let rec fall (row, col) =
    let down = cave.(row + 1).(col) in
    match down with
    | Air ->
        fall (row + 1, col)
    | Rock | Sand -> (
        let left = cave.(row + 1).(col - 1) in
        match left with
        | Air ->
            fall (row + 1, col - 1)
        | Rock | Sand -> (
            let right = cave.(row + 1).(col + 1) in
            match right with
            | Air ->
                fall (row + 1, col + 1)
            | Rock | Sand ->
                (row, col) ) )
  in
  if cave.(0).(500) = Sand then false
  else
    match fall (0, 500) with
    | row, col ->
        cave.(row).(col) <- Sand ;
        true
    | exception Invalid_argument _ ->
        false

let count =
  let count = ref 0 in
  while simulate cave do
    count := !count + 1
  done ;
  !count

let print cave =
  for row = 0 to Array.length cave - 1 do
    for col = 0 to Array.length cave.(0) - 1 do
      print_char
        (match cave.(row).(col) with Air -> '.' | Rock -> '#' | Sand -> 'o')
    done ;
    print_char '\n'
  done

let () = printf "Part 1: %d\n" count

let part2_cave =
  Array.append initial
    [|Array.init max_col (fun _ -> Air); Array.init max_col (fun _ -> Rock)|]

let part2_count =
  let count = ref 0 in
  while simulate part2_cave do
    count := !count + 1
  done ;
  !count

let () =
  print part2_cave ;
  printf "Part 2: %d" part2_count
