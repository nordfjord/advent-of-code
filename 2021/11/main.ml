let get_input () =
  Seq.of_dispenser (fun () ->
    match read_line () with
    | s -> Some s
    | exception End_of_file -> None)
  |> Array.of_seq
  |> Array.map (fun s ->
    String.to_seq s |> Seq.map (fun c -> Char.code c - Char.code '0') |> Array.of_seq)

let adjacent n m (i, j) =
  [ (i - 1, j - 1)
  ; (i - 1, j)
  ; (i - 1, j + 1)
  ; (i, j - 1)
  ; (i, j + 1)
  ; (i + 1, j - 1)
  ; (i + 1, j)
  ; (i + 1, j + 1)
  ]
  |> List.filter (fun (i, j) -> i >= 0 && i < n && j >= 0 && j < m)

let step arr =
  let n = Array.length arr in
  let m = Array.length arr.(0) in
  let flashed = Array.map (Array.map (fun _ -> false)) arr in
  let for_each f =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        f (i, j)
      done
    done
  in
  let rec flash (i, j) =
    if arr.(i).(j) > 9 && not flashed.(i).(j)
    then (
      flashed.(i).(j) <- true;
      adjacent n m (i, j)
      |> List.iter (fun (i, j) ->
        arr.(i).(j) <- arr.(i).(j) + 1;
        flash (i, j)))
  in
  for_each (fun (i, j) -> arr.(i).(j) <- arr.(i).(j) + 1);
  for_each flash;
  for_each (fun (i, j) -> if arr.(i).(j) > 9 then arr.(i).(j) <- 0);
  flashed

let count_true = Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0
let sumBy f = Array.fold_left (fun acc x -> acc + f x) 0

let rec simulate1 arr remaining count =
  if remaining = 0
  then count
  else (
    let flashed = step arr in
    let flash_count = sumBy count_true flashed in
    simulate1 arr (remaining - 1) (count + flash_count))

let rec simulate2 arr round =
  let _ = step arr in
  if Array.for_all (Array.for_all (fun b -> b = 0)) arr
  then round
  else simulate2 arr (round + 1)

let () =
  let input = get_input () in
  let copy = Array.map Array.copy in
  simulate1 (copy input) 100 0 |> Printf.printf "Part 1: %d\n";
  simulate2 (copy input) 1 |> Printf.printf "Part 2: %d\n"
