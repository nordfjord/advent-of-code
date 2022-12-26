open Printf

let cubes =
  Seq.of_dispenser (fun () ->
      match read_line () with s -> Some s | exception End_of_file -> None)
  |> Seq.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun x y z -> (x, y, z)))
  |> Array.of_seq

let points = Hashtbl.create 3000
let () = cubes |> Array.iter (fun p -> Hashtbl.add points p true)
let point_exists p = Hashtbl.mem points p

let is_adjacent (x1, y1, z1) (x2, y2, z2) =
  abs x1 - x2 + abs y1 - y2 + abs z1 - z2 = 1

let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let adjacent (x, y, z) =
  [
    (x - 1, y, z);
    (x + 1, y, z);
    (x, y - 1, z);
    (x, y + 1, z);
    (x, y, z - 1);
    (x, y, z + 1);
  ]

let count_empty_sides  cubes =
  (* a cube has 6 sides *)
  let sides = ref 0 in
  for i = 0 to Array.length cubes - 1 do
    let point = cubes.(i) in
    let covered_sides =
      adjacent point
      |> List.filter (fun point2 -> point_exists point2)
      |> List.length in
    sides := !sides + (6 - covered_sides)
  done;
  !sides
  

let part1 () = printf "Part 1: %d\n" (count_empty_sides  cubes)
let () = part1 ()
