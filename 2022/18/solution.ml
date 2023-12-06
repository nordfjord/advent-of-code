open Printf

let cubes =
  Seq.of_dispenser (fun () ->
    match read_line () with
    | s -> Some s
    | exception End_of_file -> None)
  |> Seq.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun x y z -> (x, y, z)))
  |> Array.of_seq

type block =
  | Lava
  | Air

let size = 23
let points = Array.init size (fun _ -> Array.init size (fun _ -> Array.make size Air))

let is_in_bounds (x, y, z) =
  0 <= x && x < size && 0 <= y && y < size && 0 <= z && z < size

let () = cubes |> Array.iter (fun (x, y, z) -> points.(x).(y).(z) <- Lava)

let is_lava (x, y, z) =
  if is_in_bounds (x, y, z) then points.(x).(y).(z) = Lava else false

let adjacent (x, y, z) =
  [ (x - 1, y, z)
  ; (x + 1, y, z)
  ; (x, y - 1, z)
  ; (x, y + 1, z)
  ; (x, y, z - 1)
  ; (x, y, z + 1)
  ]

let count_empty_sides cubes =
  (* a cube has 6 sides *)
  cubes
  |> Array.fold_left
       (fun sides point ->
         let covered_sides = adjacent point |> List.filter is_lava |> List.length in
         sides + (6 - covered_sides))
       0

let part1 () = printf "Part 1: %d\n%!" (count_empty_sides cubes)

(*
   The test data has an air pocket at point 2,2,5
   This means we overcounted the sides by 6

   Naively, maybe we can find all air pockets and do part1 - (pockets * 6)

   Naive won't work because if two air blocks are next to each other we won't
   detect them

   Perhaps we can do a recurive search for each adjacent block to check if they are also air pockets?

   Final solution:
   Iterate through every possible point in the coordinate space (0,0,0) - (20,20,20)
   For each point if it is air and has at least one lava next to it then
   attempt to recursively expand the air pocket. If we hit the edge of the map
   we exit early.

   Finally once we have the set of air blocks inside the lava drop, we fill them
   in and run the part1 solution again
*)

module PointSet = Set.Make (struct
    type t = int * int * int

    let compare = compare
  end)

let rec expand_airpocket_aux to_visit visited =
  match to_visit with
  | [] -> visited
  | point :: to_visit ->
    (* we're outside the lava drop, exit early *)
    if not (is_in_bounds point)
    then PointSet.empty (* can't go further, it's lava *)
    else if is_lava point || PointSet.mem point visited
    then expand_airpocket_aux to_visit visited
    else
      expand_airpocket_aux
        (* I'm placing the adjacent points first in the hopes that it brings us
           to the edge of the map faster in case we're outside the drop *)
        (List.append (adjacent point) to_visit)
        (PointSet.add point visited)

let expand_airpocket point = expand_airpocket_aux [ point ] PointSet.empty

let find_air_pockets () =
  let air_pockets = ref PointSet.empty in
  for x = 0 to 20 do
    for y = 0 to 20 do
      for z = 0 to 20 do
        let point = (x, y, z) in
        let is_air = not (is_lava point) in
        let has_adjacent_lava = adjacent point |> List.exists is_lava in
        if is_air && has_adjacent_lava
        then air_pockets := PointSet.union !air_pockets (expand_airpocket (x, y, z))
      done
    done
  done;
  !air_pockets

let part2 () =
  let pockets = find_air_pockets () |> PointSet.to_seq |> Array.of_seq in
  (* If we fill in the air pockets with lava then the part 1 solution works again *)
  pockets |> Array.iter (fun (x, y, z) -> points.(x).(y).(z) <- Lava);
  printf "Part 2: %d\n" (count_empty_sides (Array.append cubes pockets))

let () =
  part1 ();
  part2 ()
