open Base
open Stdio

let sw = Prelude.Stopwatch.start ()
let width = 5
let height = 5

module BitArray = struct
  type t = int [@@deriving hash, compare, sexp]

  let empty = 0
  let set t x = t lor (1 lsl x)
  let toggle t x = t lxor (1 lsl x)
  let mem t x = t land (1 lsl x) <> 0
end

module State = struct
  type t = BitArray.t [@@deriving hash, compare, sexp]

  let get t (x, y) = BitArray.mem t ((y * width) + x)
  let set t (x, y) = BitArray.set t ((y * width) + x)
  let get_int t p = if get t p then 1 else 0
  let toggle t (x, y) = BitArray.toggle t ((y * width) + x)
  let size t = Int.popcount t

  let show t =
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if get t (x, y) then printf "#" else printf "."
      done;
      printf "\n"
    done;
    printf "\n"
  [@@ocaml.warning "-32"]
end

let initial_state =
  In_channel.input_lines stdin
  |> List.foldi ~init:BitArray.empty ~f:(fun y acc line ->
    String.foldi line ~init:acc ~f:(fun x acc c ->
      if Char.equal c '#' then State.set acc (x, y) else acc))

let simulate state =
  (* - A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
     - An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it. *)
  let adjacent (x, y) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.filter ~f:(fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
    |> List.count ~f:(fun pos -> State.get state pos)
  in
  Sequence.range 0 width
  |> Sequence.cartesian_product (Sequence.range 0 height)
  |> Sequence.fold ~init:state ~f:(fun state (x, y) ->
    let count = adjacent (x, y) in
    let is_bug = State.get state (x, y) in
    if is_bug && count <> 1
    then State.toggle state (x, y)
    else if (not is_bug) && (count = 1 || count = 2)
    then State.toggle state (x, y)
    else state)

let part1 () =
  let seen = Hash_set.create (module Int) in
  let rec loop state =
    if Hash_set.mem seen state
    then state
    else (
      Hash_set.add seen state;
      let next_state = simulate state in
      loop next_state)
  in
  loop initial_state

let part2 () =
  let levels = Hashtbl.create (module Int) in
  Hashtbl.set levels ~key:0 ~data:initial_state;
  let get_level levels lvl =
    match Hashtbl.find levels lvl with
    | Some state -> state
    | None -> 0
  in
  let simulate_level inner middle outer =
    Sequence.range 0 width
    |> Sequence.cartesian_product (Sequence.range 0 height)
    |> Sequence.filter ~f:(fun (x, y) -> not (x = 2 && y = 2))
    |> Sequence.fold ~init:0 ~f:(fun state (x, y) ->
      let count =
        [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
        |> List.filter ~f:(fun (x, y) ->
          x >= 0 && x < width && y >= 0 && y < height && not (x = 2 && y = 2))
        |> List.count ~f:(fun p -> State.get middle p)
      in
      let count =
        count
        +
        if x = 0
        then State.get_int outer (1, 2)
        else if x = 4
        then State.get_int outer (3, 2)
        else 0
      in
      let count =
        count
        +
        if y = 0
        then State.get_int outer (2, 1)
        else if y = 4
        then State.get_int outer (2, 3)
        else 0
      in
      let count =
        count
        +
        match (x, y) with
        | 1, 2 ->
          Sequence.range 0 height |> Sequence.count ~f:(fun y -> State.get inner (0, y))
        | 3, 2 ->
          Sequence.range 0 height |> Sequence.count ~f:(fun y -> State.get inner (4, y))
        | 2, 1 ->
          Sequence.range 0 width |> Sequence.count ~f:(fun x -> State.get inner (x, 0))
        | 2, 3 ->
          Sequence.range 0 width |> Sequence.count ~f:(fun x -> State.get inner (x, 4))
        | _ -> 0
      in
      let is_bug = State.get middle (x, y) in
      if is_bug
      then if count = 1 then State.set state (x, y) else state
      else if count = 1 || count = 2
      then State.set state (x, y)
      else state)
  in
  let rec aux levels min max count =
    if count = 0
    then
      Hashtbl.fold levels ~init:0 ~f:(fun ~key:_ ~data:state acc ->
        acc + State.size state)
    else (
      let next_levels = Hashtbl.create (module Int) in
      for lvl = min - 1 to max + 1 do
        let inner = get_level levels (lvl - 1) in
        let outer = get_level levels (lvl + 1) in
        let state = get_level levels lvl in
        let next_state = simulate_level inner state outer in
        if next_state <> 0 then Hashtbl.set next_levels ~key:lvl ~data:next_state
      done;
      aux next_levels (min - 1) (max + 1) (count - 1))
  in
  aux levels 0 0 200

let () =
  Prelude.Runner.run part1 part2;
  printf "Execution time: %.3f ms\n" (sw ())
