open Base
open Stdio

let lines = In_channel.input_lines stdin

module Parse = struct
  open Angstrom

  let int = take_while1 Char.is_digit >>| Int.of_string
  let tilde = char '~'
  let comma = char ','

  let coord_range =
    let* x1 = int <* comma in
    let* y1 = int <* comma in
    let* z1 = int <* tilde in
    let* x2 = int <* comma in
    let* y2 = int <* comma in
    let* z2 = int in
    return ((x1, y1, z1), (x2, y2, z2))
end

module Block = struct
  type t = (int * int * int) * (int * int * int) [@@deriving sexp, compare, equal, hash]

  let z1 ((_, _, z), (_, _, _)) = z
  let z2 ((_, _, _), (_, _, z)) = z
  let fall ((x1, y1, z1), (x2, y2, z2)) = ((x1, y1, z1 - 1), (x2, y2, z2 - 1))

  let intersects_2d ((x1, y1, _), (x2, y2, _)) ((x1', y1', _), (x2', y2', _)) =
    let x_intersects = x1' <= x2 && x2' >= x1 in
    let y_intersects = y1' <= y2 && y2' >= y1 in
    x_intersects && y_intersects
end

let coord_ranges =
  lines
  |> List.map ~f:(Angstrom.parse_string ~consume:All Parse.coord_range)
  |> List.map ~f:Result.ok_or_failwith

let rec gravity blocks =
  let is_supported block =
    Block.z1 block = 1
    || List.exists blocks ~f:(fun block' ->
      Block.z2 block' = Block.z1 block - 1 && Block.intersects_2d block block')
  in
  let fallen =
    List.map blocks ~f:(fun block ->
      match is_supported block with
      | true -> block
      | false -> Block.fall block)
  in
  if List.equal Block.equal blocks fallen then blocks else gravity fallen

let graph blocks =
  let find_supporting_blocks block =
    List.filter blocks ~f:(fun block' ->
      (not (Block.equal block block'))
      (* the top of block' is directly underneath block *)
      && Block.z2 block' = Block.z1 block - 1
      && Block.intersects_2d block block')
  in
  let graph = Hashtbl.create (module Block) in
  blocks
  |> List.iter ~f:(fun block ->
    Hashtbl.add graph ~key:block ~data:[] |> ignore;
    List.iter (find_supporting_blocks block) ~f:(fun supporting_block ->
      Hashtbl.add_multi graph ~key:block ~data:supporting_block));
  graph

let reverse_graph graph =
  let reverse_graph = Hashtbl.create (module Block) in
  Hashtbl.iteri graph ~f:(fun ~key:block ~data:supported_by ->
    Hashtbl.add reverse_graph ~key:block ~data:[] |> ignore;
    List.iter supported_by ~f:(fun supported_block ->
      Hashtbl.add_multi reverse_graph ~key:supported_block ~data:block));
  reverse_graph

(* A block would fall if all of its supporting blocks have disintigrated *)
let count_blocks_that_would_fall supported supports block =
  let disintigrated = Hash_set.of_list (module Block) [ block ] in
  let rec loop block =
    let blocks_supported = Hashtbl.find_multi supports block in
    List.iter blocks_supported ~f:(fun block ->
      let blocks_supporting = Hashtbl.find_multi supported block in
      if List.for_all blocks_supporting ~f:(Hash_set.mem disintigrated)
      then Hash_set.add disintigrated block);
    List.iter blocks_supported ~f:loop
  in
  loop block;
  Hash_set.length disintigrated - 1

let () =
  let blocks = gravity coord_ranges in
  (* Block 'k is supported by blocks 'v *)
  let block_supported_by = graph blocks in
  (* Block 'k supports blocks 'v *)
  let block_supports = reverse_graph block_supported_by in
  let is_supported_by_more_than_one_block block =
    let supports = Hashtbl.find_multi block_supported_by block in
    List.length supports > 1
  in
  Hashtbl.fold block_supports ~init:0 ~f:(fun ~key:_ ~data:supported_by acc ->
    let all_blocks_supported_by_another =
      List.for_all supported_by ~f:is_supported_by_more_than_one_block
    in
    if all_blocks_supported_by_another then acc + 1 else acc)
  |> printf "%d\n";
  Hashtbl.fold block_supports ~init:0 ~f:(fun ~key:block ~data:_ acc ->
    count_blocks_that_would_fall block_supported_by block_supports block + acc)
  |> printf "%d\n"
