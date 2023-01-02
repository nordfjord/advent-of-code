open Printf
open Prelude
open Prelude.Computer

let input =
  let fh = open_in "./input.txt" in
  let line = input_line fh in
  close_in fh;
  line

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

let rec chunk size (seq : 'a Seq.t) () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) -> Seq.Cons (Seq.cons x (Seq.take (size - 1) xs), chunk size seq)

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
[@@deriving show]

let get_tile m t = Hashtbl.find_opt m t |> Option.value ~default:Unknown

let possible_moves map loc =
  [ 1; 2; 3; 4 ]
  |> List.filter (fun l ->
       let l = Point.(loc + direction_vector l) in
       match get_tile map l with
       | Unknown | OxygenSystem | Floor -> true
       | _ -> false)

let path_to_location path =
  path |> List.fold_left (fun loc d -> Point.(loc + direction_vector d)) (0, 0)

let bfs map =
  let q = Queue.create () in
  q |> Queue.add [];
  Hashtbl.add map (0, 0) Floor;
  let result = ref [] in
  while not (Queue.is_empty q) do
    let path = Queue.take q in
    let location = path_to_location path in
    let tile = get_tile map location in
    if tile = OxygenSystem
    then result := path
    else
      [ 1; 2; 3; 4 ]
      |> List.iter (fun d ->
           let robot = robot (get_program input) in
           path |> List.iter (fun d -> robot d |> ignore);
           let result = robot d in
           let loc = Point.(location + direction_vector d) in
           if Hashtbl.mem map loc
           then ()
           else (
             match result with
             | 0 -> Hashtbl.replace map loc Wall
             | 1 ->
               Hashtbl.replace map loc Floor;
               Queue.add (path @ [ d ]) q
             | 2 ->
               Hashtbl.replace map loc OxygenSystem;
               Queue.add (path @ [ d ]) q
             | _ -> failwith "Unexpected output"))
  done;
  !result

let part1 () =
  let map = Hashtbl.create 3000 in
  let result = bfs map in
  printf "Part 1: %d\n" (List.length result);
  for i = -19 to 21 do
    for j = -21 to 19 do
      match Hashtbl.find_opt map (i, j) with
      | Some Wall -> print_char '#'
      | Some Floor when (i, j) = (0, 0) -> print_char 'S'
      | Some Floor -> print_char '.'
      | Some OxygenSystem -> print_char 'O'
      | None | Some Unknown -> print_char '?'
    done;
    print_char '\n'
  done;
  map

let bfs map start =
  let q = Queue.create () in
  q |> Queue.add (start, 0);
  let visited = Hashtbl.create 3000 in
  Hashtbl.add visited start ();
  let result = ref 0 in
  while not (Queue.is_empty q) do
    let location, minutes = Queue.take q in
    result := max minutes !result;
    [ up; down; left; right ]
    |> List.iter (fun d ->
         let loc = Point.(location + d) in
         if Hashtbl.mem visited loc
         then ()
         else (
           match get_tile map loc with
           | Wall | Unknown -> ()
           | Floor | OxygenSystem ->
             Hashtbl.add visited loc ();
             Queue.add (loc, minutes + 1) q))
  done;
  !result

let part2 map =
  let oxygen_loc =
    Hashtbl.to_seq map
    |> Seq.find_map (fun (k, v) -> if v = OxygenSystem then Some k else None)
    |> Option.get
  in
  bfs map oxygen_loc |> printf "Part 2: %d\n"

let () = part1 () |> part2
