open Base
open Stdio

let parse_line line =
  Stdlib.Scanf.sscanf line "p=%d,%d v=%d,%d" (fun px py vx vy -> ((px, py), (vx, vy)))

let robots = In_channel.input_lines stdin |> List.map ~f:parse_line

let max_x, max_y =
  List.fold robots ~init:(0, 0) ~f:(fun (x, y) ((px, py), _) -> (max x px, max y py))

let max_x, max_y = (max_x + 1, max_y + 1)
let half_x = max_x / 2
let half_y = max_y / 2

let simulate_robot ((px, py), (vx, vy)) =
  (((px + vx) % max_x, (py + vy) % max_y), (vx, vy))

let rec simulate_all n robots =
  if n = 0 then robots else simulate_all (n - 1) (List.map robots ~f:simulate_robot)

let quadrant (px, py) =
  if 0 <= px && px < half_x && 0 <= py && py < half_y
  then 1
  else if half_x < px && px <= max_x && 0 <= py && py < half_y
  then 2
  else if half_x < px && px <= max_x && half_y < py && py <= max_y
  then 3
  else if 0 <= px && px < half_x && half_y < py && py <= max_y
  then 4
  else 0

let print_map robots =
  for y = 0 to max_y - 1 do
    for x = 0 to max_x - 1 do
      (match List.count robots ~f:(fun ((px, py), _) -> px = x && py = y) with
       | 0 -> "."
       | _ -> "#")
      |> printf "%s"
    done;
    printf "\n%!"
  done

let safety_score robots =
  let res = Array.create ~len:5 0 in
  List.iter robots ~f:(fun (p, _) ->
    let q = quadrant p in
    res.(q) <- res.(q) + 1);
  res.(0) <- 1;
  Array.fold res ~init:1 ~f:(fun sum q -> sum * q)

let part1 robots =
  let robots = simulate_all 100 robots in
  safety_score robots

let part2 robots =
  let robots = ref robots in
  let xmas_seconds = ref 0 in
  let score = ref Int.max_value in
  let robots_xmas = ref !robots in
  for i = 0 to 10000 do
    robots := List.map !robots ~f:simulate_robot;
    let new_score = safety_score !robots in
    if new_score < !score
    then (
      score := new_score;
      xmas_seconds := i;
      robots_xmas := !robots)
  done;
  printf "\nAfter %d seconds:\n" !xmas_seconds;
  print_map !robots_xmas;
  !xmas_seconds

let () =
  let p1, p2 = (part1 robots, part2 robots) in
  printf "Part 1: %d\n" p1;
  printf "Part 2: %d\n" p2
