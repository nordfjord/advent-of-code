open Base
open Stdio

let lines = In_channel.input_lines stdin

let parse line =
  match String.split ~on:'-' line with
  | [ from; to_ ] -> (from, to_)
  | _ -> failwith "invalid input"

let graph = Hashtbl.create (module String)

let () =
  lines
  |> List.iter ~f:(fun line ->
    let from, to_ = parse line in
    Hashtbl.add_multi graph ~key:from ~data:to_;
    Hashtbl.add_multi graph ~key:to_ ~data:from)

let is_uppercase s = String.(s = uppercase s)

let bfs graph start is_goal part1 =
  let q = Queue.create () in
  let visited = Set.empty (module String) in
  Queue.enqueue q (start, visited, false);
  let paths = ref 0 in
  while not (Queue.is_empty q) do
    let node, visited, twice = Queue.dequeue_exn q in
    let neighbours = Hashtbl.find_multi graph node in
    if is_goal node
    then paths := !paths + 1
    else
      List.iter neighbours ~f:(fun neighbour ->
        if String.(neighbour = "start")
        then ()
        else if is_uppercase neighbour
        then Queue.enqueue q (neighbour, visited, twice)
        else (
          match part1, Set.mem visited neighbour with
          | false, true when twice -> ()
          | true, true -> ()
          | _, true -> Queue.enqueue q (neighbour, Set.add visited neighbour, true)
          | _, false -> Queue.enqueue q (neighbour, Set.add visited neighbour, twice)))
  done;
  !paths

let () =
  let is_goal = String.equal "end" in
  bfs graph "start" is_goal true |> printf "%d\n";
  bfs graph "start" is_goal false |> printf "%d\n"
