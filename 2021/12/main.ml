let get_input () =
  Seq.of_dispenser (fun () ->
    match read_line () with
    | s -> Some s
    | exception End_of_file -> None)
  |> Seq.map (fun s ->
    match String.split_on_char '-' s with
    | [ a; b ] -> (a, b)
    | _ -> failwith "invalid input")
  |> Seq.fold_left
       (fun acc (start, end_) ->
         (match Hashtbl.find_opt acc start with
          | None -> Hashtbl.add acc start [ end_ ]
          | Some ends -> Hashtbl.replace acc start (end_ :: ends));
         (match Hashtbl.find_opt acc end_ with
          | None -> Hashtbl.add acc end_ [ start ]
          | Some starts -> Hashtbl.replace acc end_ (start :: starts));
         acc)
       (Hashtbl.create 42)

type nodelist = string list [@@deriving show]

let bfs nodes start dst =
  let q = Queue.create () in
  Queue.add [ start ] q;
  let result = ref [] in
  let visited = Hashtbl.create 42 in
  while not (Queue.is_empty q) do
    let path = Queue.take q in
    let node = List.hd path in
    if node = dst
    then result := path :: !result
    else (
      Printf.printf "node: %s, path: %s\n%!" node (show_nodelist path);
      let paths = Hashtbl.find_opt nodes node |> Option.value ~default:[] in
      paths
      |> List.map (fun next -> next :: path)
      |> List.filter (fun p -> not (Hashtbl.mem visited p))
      |> List.iter (fun path ->
        Hashtbl.add visited path ();
        Queue.add path q))
  done;
  !result

type result = string list list [@@deriving show]

let () =
  let input = get_input () in
  Hashtbl.length input |> Printf.printf "Part 1: %d\n%!";
  let result = bfs input "start" "end" in
  Printf.printf "Part 1: %s\n%!" (show_result result);
  result |> List.length |> Printf.printf "Part 1: %d\n"
