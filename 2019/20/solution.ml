open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()
let lines = In_channel.input_lines stdin |> Array.of_list

module IntPair = struct
  type t = int * int [@@deriving compare, hash, sexp, equal]
end

let init, finish, portals =
  let p = Hashtbl.create (module String) in
  let add_portal name pos =
    match Hashtbl.find p name with
    | Some (Some a, None) -> Hashtbl.set p ~key:name ~data:(Some a, Some pos)
    | Some (None, Some a) -> Hashtbl.set p ~key:name ~data:(Some pos, Some a)
    | Some (Some _, Some _) -> failwith "Portal already has two positions"
    | Some (None, None) | None -> Hashtbl.set p ~key:name ~data:(Some pos, None)
  in
  let height = Array.length lines in
  let width = String.length lines.(0) in
  let is_uppercase c = Char.(c >= 'A' && c <= 'Z') in
  let try_add_portal x y =
    if is_uppercase lines.(y).[x]
    then (* vertical portal *)
      if is_uppercase lines.(y + 1).[x]
      then (
        let name = String.of_char_list [ lines.(y).[x]; lines.(y + 1).[x] ] in
        let pos_y =
          if y + 2 < height && Char.(lines.(y + 2).[x] = '.')
          then y + 2
          else if y - 1 >= 0 && Char.(lines.(y - 1).[x] = '.')
          then y - 1
          else failwith "No portal position found"
        in
        add_portal name (x, pos_y))
      else if is_uppercase lines.(y).[x + 1]
      then (
        (* horizontal portal *)
        let name = String.of_char_list [ lines.(y).[x]; lines.(y).[x + 1] ] in
        let pos_x =
          if x + 2 < width && Char.(lines.(y).[x + 2] = '.')
          then x + 2
          else if x - 1 >= 0 && Char.(lines.(y).[x - 1] = '.')
          then x - 1
          else failwith "No portal position found"
        in
        add_portal name (pos_x, y))
  in
  for y = 0 to height - 2 do
    for x = 0 to width - 2 do
      try_add_portal x y
    done
  done;
  let start = Hashtbl.find_exn p "AA" |> fst |> Option.value_exn in
  let finish = Hashtbl.find_exn p "ZZ" |> fst |> Option.value_exn in
  let portals =
    Hashtbl.filter_map p ~f:(fun (a, b) ->
      match (a, b) with
      | Some a, Some b -> Some (a, b)
      | _ -> None)
    |> Hashtbl.data
    |> List.concat_map ~f:(fun (a, b) -> [ (a, b); (b, a) ])
    |> Hashtbl.of_alist_exn (module IntPair)
  in
  (start, finish, portals)

let moves portals (x, y) =
  let portal = Hashtbl.find portals (x, y) in
  let portal_moves =
    match portal with
    | Some dest -> [ dest ]
    | None -> []
  in
  [ (x, y + 1); (x + 1, y); (x, y - 1); (x - 1, y) ]
  |> List.filter ~f:(fun (nx, ny) ->
    ny >= 0
    && ny < Array.length lines
    && nx >= 0
    && nx < String.length lines.(ny)
    && Char.(lines.(ny).[nx] = '.'))
  |> List.append portal_moves

let bfs start finish =
  let visited = Hash_set.create (module IntPair) in
  let q = Queue.create () in
  Queue.enqueue q (start, 0);
  Hash_set.add visited start;
  let rec aux () =
    match Queue.dequeue q with
    | None -> 0
    | Some (pos, dist) ->
      if IntPair.equal pos finish
      then dist
      else (
        moves portals pos
        |> List.iter ~f:(fun pos ->
          if not (Hash_set.mem visited pos)
          then (
            Hash_set.add visited pos;
            Queue.enqueue q (pos, dist + 1)));
        aux ())
  in
  aux ()

module State = struct
  type t = (int * int) * int [@@deriving compare, hash, sexp, equal]
end

let part1 () = bfs init finish

let is_inner_portal (x, y) =
  x > 3 && y > 3 && x < String.length lines.(0) - 3 && y < Array.length lines - 3

let moves level (x, y) =
  let portal = Hashtbl.find portals (x, y) in
  let inner = is_inner_portal (x, y) in
  let portal_moves =
    match portal with
    | Some _ when level = 0 && not inner -> []
    | Some dest when inner -> [ (dest, level + 1) ]
    | Some dest -> [ (dest, level - 1) ]
    | None -> []
  in
  [ (x, y + 1); (x + 1, y); (x, y - 1); (x - 1, y) ]
  |> List.filter ~f:(fun (nx, ny) ->
    ny >= 0
    && ny < Array.length lines
    && nx >= 0
    && nx < String.length lines.(ny)
    && Char.(lines.(ny).[nx] = '.'))
  |> List.map ~f:(fun pos -> (pos, level))
  |> List.append portal_moves

let part2 () =
  let bfs start finish =
    let visited = Hash_set.create (module State) in
    let q = Queue.create () in
    Queue.enqueue q ((start, 0), 0);
    Hash_set.add visited (start, 0);
    let rec aux () =
      match Queue.dequeue q with
      | None -> 0
      | Some ((pos, level), dist) ->
        if IntPair.equal pos finish && level = 0
        then dist
        else (
          let next_moves = moves level pos in
          next_moves
          |> List.iter ~f:(fun (next_pos, next_level) ->
            let next_state = (next_pos, next_level) in
            if not (Hash_set.mem visited next_state)
            then (
              Hash_set.add visited next_state;
              Queue.enqueue q (next_state, dist + 1)));
          aux ())
    in
    aux ()
  in
  bfs init finish

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
