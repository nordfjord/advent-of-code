open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()
let lines = In_channel.input_lines stdin |> Array.of_list

module Tile = struct
  type t =
    | Free
    | Key of int
    | Door of int
  [@@deriving hash, compare, equal, sexp]
end

module BitArray = struct
  type t = int [@@deriving hash, compare, equal, sexp, show]

  let set ba index = ba lor (1 lsl index)
  let remove ba index = ba land lnot (1 lsl index)
  let mem ba index = ba land (1 lsl index) <> 0
  let merge ba1 ba2 = ba1 lor ba2
end

module Pos = struct
  type t = int * int [@@deriving hash, sexp, compare, equal]
end

module Map = struct
  type t =
    { tiles : Tile.t Hashtbl.M(Pos).t
    ; keys : int
    ; pos : Pos.t
    }

  let from_input lines =
    let tiles = Hashtbl.create (module Pos) in
    let keys = ref 0 in
    let pos = ref (0, 0) in
    Array.iteri lines ~f:(fun y line ->
      String.iteri line ~f:(fun x c ->
        let p = (x, y) in
        match c with
        | '#' -> ()
        | '.' -> Hashtbl.set tiles ~key:p ~data:Tile.Free
        | '@' ->
          Hashtbl.set tiles ~key:p ~data:Free;
          pos := p
        | 'a' .. 'z' as k ->
          Hashtbl.set tiles ~key:p ~data:(Key (Char.to_int k - Char.to_int 'a'));
          keys := BitArray.set !keys (Char.to_int k - Char.to_int 'a')
        | 'A' .. 'Z' as d ->
          Hashtbl.set tiles ~key:p ~data:(Door (Char.to_int d - Char.to_int 'A'))
        | _ -> ()));
    { tiles; keys = !keys; pos = !pos }

  let find_keys tiles pos =
    let seen = Hash_set.create (module Pos) in
    let rec aux pos =
      Hash_set.add seen pos;
      let x, y = pos in
      let keys =
        match Hashtbl.find tiles pos with
        | Some (Tile.Key k) -> 1 lsl k
        | _ -> 0
      in
      let deltas = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
      List.fold deltas ~init:keys ~f:(fun keys (dx, dy) ->
        let np = (x + dx, y + dy) in
        if (not (Hash_set.mem seen np)) && Hashtbl.mem tiles np
        then BitArray.merge keys (aux np)
        else keys)
    in
    aux pos

  let partition map =
    let x, y = map.pos in
    let tiles = Hashtbl.copy map.tiles in
    [ (0, 0); (0, -1); (0, 1); (-1, 0); (1, 0) ]
    |> List.iter ~f:(fun (dx, dy) ->
      let np = (x + dx, y + dy) in
      Hashtbl.remove tiles np);
    [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
    |> List.map ~f:(fun (dx, dy) ->
      let new_pos = (x + dx, y + dy) in
      Hashtbl.set tiles ~key:new_pos ~data:Tile.Free;
      { tiles; pos = new_pos; keys = find_keys tiles new_pos })
end

module State = struct
  type t =
    { pos : Pos.t
    ; missing_keys : BitArray.t
    }
  [@@deriving hash, sexp, compare, equal]

  let seed (map : Map.t) = { pos = map.pos; missing_keys = map.keys }

  let next state (map : Map.t) =
    let deltas = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
    List.filter_map deltas ~f:(fun (dx, dy) ->
      let x, y = state.pos in
      let np = (x + dx, y + dy) in
      match Hashtbl.find map.tiles np with
      | Some (Tile.Door k) when BitArray.mem state.missing_keys k -> None
      | Some (Tile.Door _) -> Some { state with pos = np }
      | Some (Tile.Key k) ->
        Some { pos = np; missing_keys = BitArray.remove state.missing_keys k }
      | Some Tile.Free -> Some { state with pos = np }
      | None -> None)
end

let bfs map =
  let steps_to = Hashtbl.create (module State) in
  let seed = State.seed map in
  let q = Queue.create () in
  Queue.enqueue q seed;
  Hashtbl.set steps_to ~key:seed ~data:0;
  let rec aux () =
    match Queue.dequeue q with
    | None -> failwith "no solution"
    | Some state ->
      (match Hashtbl.find steps_to state with
       | None -> 0
       | Some steps ->
         if state.missing_keys = 0
         then steps
         else (
           State.next state map
           |> List.iter ~f:(fun next_state ->
             if not (Hashtbl.mem steps_to next_state)
             then (
               Hashtbl.set steps_to ~key:next_state ~data:(steps + 1);
               Queue.enqueue q next_state));
           aux ()))
  in
  aux ()

let map = Map.from_input lines
let part1 () = bfs map
let part2 () = map |> Map.partition |> List.sum (module Int) ~f:bfs

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
