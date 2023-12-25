open Base
open Stdio

let parse_line l =
  match Str.split (Str.regexp_string ": ") l with
  | [ left; right ] -> (left, String.split right ~on:' ')
  | _ -> failwith "parse error"

let connections = In_channel.input_lines stdin |> List.map ~f:parse_line

let to_graph connections =
  let g = Hashtbl.create (module String) in
  List.iter connections ~f:(fun (left, rights) ->
    List.iter rights ~f:(fun right ->
      Hashtbl.add_multi g ~key:left ~data:right;
      Hashtbl.add_multi g ~key:right ~data:left));
  g

let () =
  printf "graph G {\n";
  connections
  |> List.iter ~f:(fun (left, rights) ->
    List.iter rights ~f:(fun right -> printf "  %s -- %s;\n" left right));
  printf "}\n"

let bfs g start =
  let visited = Hash_set.create (module String) in
  let queue = Queue.create () in
  Queue.enqueue queue start;
  Hash_set.add visited start;
  while not (Queue.is_empty queue) do
    let node = Queue.dequeue_exn queue in
    List.iter (Hashtbl.find_multi g node) ~f:(fun child ->
      if not (Hash_set.mem visited child)
      then (
        Hash_set.add visited child;
        Queue.enqueue queue child))
  done;
  visited |> Hash_set.length

let () =
  let g = to_graph connections in
  let connections = [ ("qnv", "mnh"); ("ljh", "tbg"); ("mfs", "ffv") ] in
  List.iter connections ~f:(fun (left, right) ->
    printf "Removing %s -- %s\n" left right;
    Hashtbl.update g left ~f:(function
      | None -> []
      | Some rights -> List.filter rights ~f:(Fn.non (String.equal right)));
    Hashtbl.update g right ~f:(function
      | None -> []
      | Some rights -> List.filter rights ~f:(Fn.non (String.equal left))));
  printf "%d\n" (bfs g "qnv" * bfs g "mnh")
