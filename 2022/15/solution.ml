open Printf

let lines =
  Seq.of_dispenser (fun () ->
      match read_line () with x -> Some x | exception End_of_file -> None)

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parsed =
  lines
  |> Seq.map (fun line ->
         Scanf.sscanf line
           "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
           (fun x1 y1 x2 y2 -> ((x1, y1), (x2, y2))))
  |> List.of_seq

module Range = struct
  type t = int * int

  let overlaps (start1, end1) (start2, end2) =
    (start1 >= start2 && start1 <= end2)
    || (end1 >= start2 && end1 <= end2)
    || (start2 >= start1 && start2 <= end1)
    || (end2 >= start1 && end2 <= end1)
    || start1 == end2 + 1
    || end1 + 1 == start2

  let clamp (minX, maxY) (x, y) = (max x minX, min y maxY)
  let merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)
  let size (x, y) = y - x + 1
end

module RangeSet = Set.Make (struct
  type t = Range.t

  let compare = compare
end)

let covered_range_in_row row ((x, y), (bx, by)) =
  (* The sensors range is widest at the row the sensor is on.
     For each row up or down the range diminishes by 1 cell *)
  let vertical_distance = abs (y - row) in
  let scan_distance = manhattan_distance (x, y) (bx, by) in

  (* The range the sensor can cover on this row is determined
     by the scan distance of the sensor and how far away the row is *)
  let max_horizontal_distance = scan_distance - vertical_distance in
  if max_horizontal_distance >= 0 then
    let range_start = x - max_horizontal_distance in
    let range_end = x + max_horizontal_distance in
    Some (range_start, range_end)
  else None

let beacons_in_row row parsed =
  parsed
  |> List.filter_map (fun (_, beacon) ->
         let _, y = beacon in
         if y = row then Some beacon else None)
  |> List.sort_uniq compare |> List.length

let merge_ranges ranges range =
  let range = ref range in
  let ranges =
    ranges |> RangeSet.to_seq
    |> Seq.fold_left
         (fun ranges r ->
           if Range.overlaps r !range then (
             range := Range.merge r !range;
             RangeSet.remove r ranges)
           else ranges)
         ranges
  in
  RangeSet.add !range ranges

let covered_ranges_in_row f row sensors =
  sensors
  |> List.fold_left
       (fun ranges value ->
         let range = covered_range_in_row row value in
         match range with
         | Some range -> merge_ranges ranges (f range)
         | None -> ranges)
       RangeSet.empty

let id x = x

let () =
  let row = Sys.argv.(1) |> int_of_string in

  let ranges = covered_ranges_in_row id row parsed in
  let beacons = beacons_in_row row parsed in
  ranges |> RangeSet.to_seq
  |> Seq.iter (fun range ->
         let size = Range.size range in
         printf "Part 1: %d\n" (size - beacons))

let () =
  let search_space = Sys.argv.(2) |> int_of_string in
  let clamp = Range.clamp (0, search_space) in
  let solution = ref None in
  let row = ref 0 in
  while !row <= search_space && !solution = None do
    let ranges = covered_ranges_in_row clamp !row parsed in
    match ranges |> RangeSet.to_seq |> List.of_seq with
    (* The solution is found once there's a gap in the covered range *)
    | (a, b) :: (c, d) :: _ ->
        solution := Some (c - 1, !row);
        printf "%d->%d %d->%d\n" a b c d
    | _ ->
        ();
        row := !row + 1
  done;
  match !solution with
  | Some (x, y) -> printf "Part 2: (%d,%d) %d\n" x y ((x * 4000000) + y)
  | None -> printf "Part2: No solution\n"
