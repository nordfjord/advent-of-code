let lines () =
  Seq.of_dispenser (fun () ->
    match read_line () with
    | s -> Some s
    | exception End_of_file -> None)

let filesystem = Hashtbl.create 1000

type line =
  | ChangeDirectory of string
  | File of int

let parse_line line =
  match line.[0] with
  | '$' ->
    (match line.[2] with
     | 'l' -> None
     | 'c' -> Scanf.sscanf line "$ cd %s" (fun s -> Some (ChangeDirectory s))
     | _ -> failwith "unknown command")
  | _ ->
    if String.starts_with ~prefix:"dir" line
    then None
    else Some (Scanf.sscanf line "%d %s" (fun i _ -> File i))

let adjust tbl f key =
  match Hashtbl.find_opt tbl key with
  | None -> Hashtbl.replace tbl key (f 0)
  | Some v -> Hashtbl.replace tbl key (f v)

let prepare_fs () =
  let directorystack = Stack.create () in
  lines ()
  |> Seq.filter_map parse_line
  |> Seq.iter (fun cmd ->
    match cmd with
    | ChangeDirectory s ->
      (match s with
       | "/" ->
         Stack.clear directorystack;
         Stack.push "/" directorystack
       | ".." -> Stack.pop directorystack |> ignore
       | s ->
         let current = Stack.top directorystack in
         Stack.push (current ^ s ^ "/") directorystack)
    | File size -> directorystack |> Stack.iter (adjust filesystem (( + ) size)))

let () =
  prepare_fs ();
  Hashtbl.to_seq_values filesystem
  |> Seq.fold_left (fun acc v -> if v < 100000 then acc + v else acc) 0
  |> Printf.printf "Part 1: %d\n";
  let total_space = 70000000 in
  let required_space = 30000000 in
  let used_space = Hashtbl.find filesystem "/" in
  let unused_space = total_space - used_space in
  let free_threshold = required_space - unused_space in
  Hashtbl.to_seq_values filesystem
  |> Seq.filter (fun v -> v > free_threshold)
  |> Seq.fold_left (fun acc v -> if v < acc then v else acc) total_space
  |> Printf.printf "Part 2: %d\n"
