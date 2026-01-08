open Base
open Stdio

let sw = Prelude.Stopwatch.start ()

let lines =
  In_channel.with_file "./input.txt" ~f:In_channel.input_line |> Option.value_exn

let computer = Computer.get_computer lines

module Room = struct
  type dir =
    | North
    | South
    | West
    | East
  [@@deriving sexp, hash, compare]

  type t =
    { name : string
    ; description : string
    ; doors : dir list
    ; items : string list
    }
  [@@deriving sexp, hash, compare]
end

module Command = struct
  type t =
    | Dir of Room.dir
    | Take of string
    | Drop of string
    | Inv

  let to_string = function
    | Dir Room.North -> "north"
    | Dir Room.South -> "south"
    | Dir Room.West -> "west"
    | Dir Room.East -> "east"
    | Take item -> "take " ^ item
    | Drop item -> "drop " ^ item
    | Inv -> "inv"
end

module Parse = struct
  open Angstrom

  (* EXAMPLE:
== Engineering ==
You see a whiteboard with plans for Springdroid v2.

Doors here lead:
- west

Items here:
- antenna

Command?
  *)

  (* Items here: is optional *)

  let dir =
    choice
      [ string "north" *> return Room.North
      ; string "south" *> return Room.South
      ; string "west" *> return Room.West
      ; string "east" *> return Room.East
      ]

  let dirs_section =
    string "Doors here lead:\n" *> many (string "- " *> dir <* end_of_line) <* end_of_line

  let room_name =
    let* _ = string "== " in
    let* name = take_till (Char.equal '=') in
    let* _ = string "==\n" in
    return name

  let items_section =
    string "Items here:\n"
    *> many (string "- " *> take_while1 (fun c -> Char.(c <> '\n')) <* end_of_line)
    <* end_of_line

  let output =
    let* _ = many (char '\n') in
    let* name = room_name in
    let* description = take_while (fun c -> not (Char.equal c '\n')) <* end_of_line in
    let* _ = end_of_line in
    let* doors = dirs_section in
    let* items = option [] items_section in
    let _ = many any_char in
    let* _ = string "Command?\n" in
    return { Room.name; description; doors; items }

  let parse_output s =
    match parse_string ~consume:Prefix output s with
    | Ok room -> Some room
    | Error msg -> None
end

let input f s =
  let rec aux i f =
    match f with
    | Computer.InputRequested g ->
      if i >= String.length s
      then g (Char.to_int '\n')
      else (
        let c = Char.to_int (String.get s i) in
        aux (i + 1) (g c))
    | _ -> f
  in
  aux 0 f

module State = struct
  type t = { inventory : Hash_set.M(String).t }
end

module DirList = struct
  type t = Room.dir list [@@deriving sexp, hash, compare]
end

let perm src n =
  let rec extend remaining_count tails =
    match remaining_count with
    | 0 -> tails
    | _ ->
      (* Put an element 'src_elt' taken from all the possible elements 'src'
           in front of each possible tail 'tail' taken from 'tails',
           resulting in 'new_tails'. The elements of 'new_tails' are one
           item longer than the elements of 'tails'. *)
      let new_tails =
        List.fold src ~init:[] ~f:(fun new_tails src_elt ->
          List.fold tails ~init:new_tails ~f:(fun new_tails tail ->
            (src_elt :: tail) :: new_tails))
      in
      extend (remaining_count - 1) new_tails
  in
  extend n [ [] ]

let solve_security items c =
  let permutations = perm (Hash_set.to_list items) 4 in
  let inventory = Hash_set.to_list items in
  let rec drop_all inv c =
    match (inv, c) with
    | item :: items, Computer.InputRequested _ ->
      let cmd = Command.Drop item |> Command.to_string in
      drop_all items (input c cmd)
    | _, Computer.Out (x, f) ->
      Out_channel.output_char stdout (Char.of_int_exn x);
      drop_all inv (f ())
    | _ -> c
  in
  let rec take_all inv c =
    match (inv, c) with
    | item :: items, Computer.InputRequested _ ->
      let cmd = Command.Take item |> Command.to_string in
      take_all items (input c cmd)
    | _, Computer.Out (x, f) ->
      Out_channel.output_char stdout (Char.of_int_exn x);
      take_all inv (f ())
    | _ -> c
  in
  let rec try_permutations perm c =
    match perm with
    | [] -> c
    | inv :: tl ->
      let c = drop_all inventory c in
      let c = take_all inv c in
      let c = input c "east" in
      try_permutations tl c
  in
  try_permutations permutations c

let run_ascii c =
  let buffer = Buffer.create 2048 in
  let banned_items =
    Hash_set.of_list
      (module String)
      [ "giant electromagnet"; "infinite loop"; "molten lava"; "escape pod"; "photons" ]
  in
  let inventory = Hash_set.create (module String) in
  let commands =
    Queue.of_list
      [ "north"
      ; "north"
      ; "east"
      ; "west"
      ; "west"
      ; "east"
      ; "south"
      ; "east"
      ; "north"
      ; "south"
      ; "east"
      ; "north"
      ; "south"
      ; "west"
      ; "west"
      ; "south"
      ; "west"
      ; "west"
      ; "north"
      ; "south"
      ; "west"
      ; "east"
      ; "east"
      ; "east"
      ; "north"
      ; "south"
      ; "east"
      ; "east"
      ; "north"
      ; "south"
      ; "east"
      ; "east"
      ; "north"
      ]
  in
  let rec run_ascii res room =
    match res with
    | Computer.Out (x, f) ->
      Buffer.add_char buffer (Char.of_int_exn x);
      run_ascii (f ()) room
    | Computer.InputRequested _ ->
      let contents = Buffer.contents buffer in
      printf "%s%!" contents;
      let parsed = Parse.parse_output contents in
      Buffer.clear buffer;
      let room =
        match parsed with
        | None -> room
        | Some room -> room
      in
      let items_to_take =
        room.items |> List.filter ~f:(fun item -> not (Hash_set.mem banned_items item))
      in
      (match items_to_take with
       | item :: tl ->
         Hash_set.add inventory item;
         let cmd = Command.Take item |> Command.to_string in
         run_ascii (input res cmd) { room with items = tl }
       | [] ->
         if String.equal room.name "Security Checkpoint "
         then (
           printf "Attempting to solve security checkpoint...\n";
           let res = solve_security inventory res in
           run_ascii res room)
         else (
           match Queue.dequeue commands with
           | Some cmd -> run_ascii (input res cmd) room
           | None ->
             let s = In_channel.input_line stdin |> Option.value_exn in
             run_ascii (input res s) room))
    | Computer.Halted -> ()
  in
  run_ascii c { Room.name = ""; description = ""; doors = []; items = [] }

let () =
  run_ascii (computer |> Computer.run);
  printf "Execution time: %.3f ms\n" (sw ())
