open Printf
open Prelude
open Base
open Computer

let input = In_channel.input_line Stdio.stdin |> Option.value_exn

module IntPair = struct
  type t = int * int [@@deriving compare, hash, sexp]
end

let robot program =
  let p = ref program in
  let rec get_next value =
    match !p with
    | Halted -> 99
    | InputRequested f ->
      (match value with
       | None -> failwith "Input expected"
       | Some i ->
         p := f i;
         get_next None)
    | Out (value, next) ->
      p := next ();
      value
  in
  fun inp -> get_next (Some inp)

let up = (1, 0)
let down = (-1, 0)
let left = (0, -1)
let right = (0, 1)

let direction_vector = function
  | 1 -> up
  | 2 -> down
  | 3 -> left
  | 4 -> right
  | _ -> failwith "Invalid"

type tile =
  | Unknown
  | Floor
  | Wall
  | OxygenSystem
[@@deriving show, equal]

let get_tile m t = Hashtbl.find m t |> Option.value ~default:Unknown

let path_to_location path =
  List.fold path ~f:(fun loc d -> Point.(loc + direction_vector d)) ~init:(0, 0)

let bfs map =
  let q = Queue.create () in
  Queue.enqueue q [];
  Hashtbl.set map ~key:(0, 0) ~data:Floor;
  let result = ref [] in
  while not (Queue.is_empty q) do
    let path = Queue.dequeue_exn q in
    let location = path_to_location path in
    let tile = get_tile map location in
    if [%equal: tile] tile OxygenSystem
    then result := path
    else
      [ 1; 2; 3; 4 ]
      |> List.iter ~f:(fun d ->
        let robot = robot (get_program input) in
        path |> List.iter ~f:(fun d -> robot d |> ignore);
        let result = robot d in
        let loc = Point.(location + direction_vector d) in
        if Hashtbl.mem map loc
        then ()
        else (
          match result with
          | 0 -> Hashtbl.set map ~key:loc ~data:Wall
          | 1 ->
            Hashtbl.set map ~key:loc ~data:Floor;
            Queue.enqueue q (path @ [ d ])
          | 2 ->
            Hashtbl.set map ~key:loc ~data:OxygenSystem;
            Queue.enqueue q (path @ [ d ])
          | _ -> failwith "Unexpected output"))
  done;
  !result

let part1 () =
  let map = Hashtbl.create (module IntPair) ~size:3000 in
  let result = bfs map in
  printf "Part 1: %d\n" (List.length result);
  for i = -19 to 21 do
    for j = -21 to 19 do
      match Hashtbl.find map (i, j) with
      | Some Wall -> Stdlib.print_char '#'
      | Some Floor when i = 0 && j = 0 -> Stdlib.print_char 'S'
      | Some Floor -> Stdlib.print_char '.'
      | Some OxygenSystem -> Stdlib.print_char 'O'
      | None | Some Unknown -> Stdlib.print_char '?'
    done;
    Stdlib.print_char '\n'
  done;
  map

let bfs map start =
  let q = Queue.create () in
  Queue.enqueue q (start, 0);
  let visited = Hash_set.create (module IntPair) ~size:3000 in
  Hash_set.add visited start;
  let result = ref 0 in
  while not (Queue.is_empty q) do
    let location, minutes = Queue.dequeue_exn q in
    result := max minutes !result;
    [ up; down; left; right ]
    |> List.iter ~f:(fun d ->
      let loc = Point.(location + d) in
      if Hash_set.mem visited loc
      then ()
      else (
        match get_tile map loc with
        | Wall | Unknown -> ()
        | Floor | OxygenSystem ->
          Hash_set.add visited loc;
          Queue.enqueue q (loc, minutes + 1)))
  done;
  !result

let part2 map =
  let oxygen_loc =
    Hashtbl.to_alist map
    |> List.find_map ~f:(fun (k, v) ->
      if [%equal: tile] v OxygenSystem then Some k else None)
    |> Option.value_exn
  in
  bfs map oxygen_loc |> printf "Part 2: %d\n"

let () = part1 () |> part2
