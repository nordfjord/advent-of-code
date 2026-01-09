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

  let to_string = function
    | Dir Room.North -> "north"
    | Dir Room.South -> "south"
    | Dir Room.West -> "west"
    | Dir Room.East -> "east"
    | Take item -> "take " ^ item
    | Drop item -> "drop " ^ item
end

module Parse = struct
  open Angstrom

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
    | Error _ -> None
end

let output c =
  let rec aux buffer c =
    match c with
    | Computer.Out (x, f) ->
      Buffer.add_char buffer (Char.of_int_exn x);
      aux buffer (f ())
    | _ -> (Buffer.contents buffer, c)
  in
  aux (Buffer.create 2048) c

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

let perm src n =
  let rec combine k list =
    if k = 0
    then Sequence.singleton []
    else (
      match list with
      | [] -> Sequence.empty
      | x :: xs ->
        let with_x = Sequence.map (combine (k - 1) xs) ~f:(fun l -> x :: l) in
        let without_x = combine k xs in
        Sequence.append with_x without_x)
  in
  combine n src

let move_to path c =
  let rec aux path c =
    match (path, c) with
    | [ dir ], Computer.InputRequested _ ->
      let cmd = Command.Dir dir |> Command.to_string in
      input c cmd
    | dir :: tl, Computer.InputRequested _ ->
      let cmd = Command.Dir dir |> Command.to_string in
      aux tl (input c cmd)
    | _, Computer.Out (_, f) -> aux path (f ())
    | _ -> c
  in
  aux path c

let solve_security items c =
  let permutations = perm (Hash_set.to_list items) 4 in
  let inventory = Hash_set.to_list items in
  let rec drop_all inv c =
    match (inv, c) with
    | item :: items, Computer.InputRequested _ ->
      let cmd = Command.Drop item |> Command.to_string in
      drop_all items (input c cmd)
    | _, Computer.Out (_, f) -> drop_all inv (f ())
    | _ -> c
  in
  let rec take_all inv c =
    match (inv, c) with
    | item :: items, Computer.InputRequested _ ->
      let cmd = Command.Take item |> Command.to_string in
      take_all items (input c cmd)
    | _, Computer.Out (_, f) -> take_all inv (f ())
    | _ -> c
  in
  Sequence.fold_until
    permutations
    ~init:c
    ~f:(fun c inv ->
      match c with
      | Computer.Halted -> Stop c
      | _ ->
        let c = drop_all inventory c in
        let c = take_all inv c in
        let c = input c "east" in
        let content, c = output c in
        if String.is_substring content ~substring:"You may proceed."
        then (
          printf "Successful inventory:\n";
          List.iter inv ~f:(fun item -> printf "- %s\n" item);
          printf "%s\n" content;
          Stop c)
        else Continue c)
    ~finish:(fun c -> c)

module Path = struct
  type t = Room.dir list [@@deriving sexp, hash, compare]

  let join dir path =
    match (path, dir) with
    | [], dir -> [ dir ]
    | Room.North :: tl, Room.South -> tl
    | Room.South :: tl, Room.North -> tl
    | Room.East :: tl, Room.West -> tl
    | Room.West :: tl, Room.East -> tl
    | _ -> dir :: path

  let reverse dir =
    match dir with
    | Room.North -> Room.South
    | Room.South -> Room.North
    | Room.East -> Room.West
    | Room.West -> Room.East

  let rec common_prefix_len a b i =
    match (a, b) with
    | [], _ | _, [] -> i
    | x :: xs, y :: ys when Room.compare_dir x y = 0 -> common_prefix_len xs ys (i + 1)
    | _ -> i

  let path_to current destination =
    let current = List.rev current in
    let destination = List.rev destination in
    let prefix_len = common_prefix_len current destination 0 in
    let back = List.map ~f:reverse (List.drop current prefix_len) in
    let forward = List.drop destination prefix_len in
    List.rev (back @ forward)
end

let run_ascii c =
  let buffer = Buffer.create 1024 in
  let banned_items =
    Hash_set.of_list
      (module String)
      [ "giant electromagnet"; "infinite loop"; "molten lava"; "escape pod"; "photons" ]
  in
  let inventory = Hash_set.create (module String) in
  let visited = Hash_set.create (module Path) in
  let commands = Queue.create () in
  let rec run_ascii res room path =
    match res with
    | Computer.Out (x, f) ->
      Buffer.add_char buffer (Char.of_int_exn x);
      run_ascii (f ()) room path
    | Computer.InputRequested _ ->
      let contents = Buffer.contents buffer in
      let parsed = Parse.parse_output contents in
      Buffer.clear buffer;
      let room =
        match parsed with
        | None -> room
        | Some room ->
          printf "Entered room: %s\n" room.name;
          room
      in
      let items_to_take =
        room.items |> List.filter ~f:(fun item -> not (Hash_set.mem banned_items item))
      in
      (match items_to_take with
       | item :: tl ->
         Hash_set.add inventory item;
         let cmd = Command.Take item |> Command.to_string in
         run_ascii (input res cmd) { room with items = tl } path
       | [] ->
         if String.equal room.name "Security Checkpoint "
         then (
           printf "Attempting to solve security checkpoint...\n";
           let res = solve_security inventory res in
           run_ascii res room path)
         else (
           room.doors
           |> List.iter ~f:(fun dir ->
             let new_path = Path.join dir path in
             if not (Hash_set.mem visited new_path)
             then (
               Hash_set.add visited new_path;
               Queue.enqueue commands new_path));
           match Queue.dequeue commands with
           | Some p ->
             let moves = Path.path_to path p in
             let c = move_to moves res in
             run_ascii c room p
           | None ->
             let s = In_channel.input_line stdin |> Option.value_exn in
             run_ascii (input res s) room path))
    | Computer.Halted -> ()
  in
  run_ascii c { Room.name = ""; description = ""; doors = []; items = [] } []

let run_term c =
  let rec aux c =
    match c with
    | Computer.Out (x, f) ->
      Out_channel.output_char stdout (Char.of_int_exn x);
      aux (f ())
    | Computer.InputRequested _ ->
      Out_channel.flush stdout;
      let line = In_channel.input_line stdin |> Option.value_exn in
      aux (input c line)
    | Computer.Halted -> ()
  in
  aux c

let () =
  run_term (computer |> Computer.run);
  (* run_ascii (computer |> Computer.run); *)
  printf "Execution time: %.3f ms\n" (sw ())
