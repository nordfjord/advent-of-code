open Base
open Stdio

let lines =
  In_channel.input_all stdin
  |> Str.split (Str.regexp_string "\n\n")
  |> List.map ~f:(fun s ->
    Stdlib.Scanf.sscanf
      s
      "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d"
      (fun ax ay bx by px py -> ((ax, ay), (bx, by), (px, py))))

let solve (ax, ay) (bx, by) (px, py) =
  let determinant = (ax * by) - (ay * bx) in
  let a = ((px * by) - (py * bx)) / determinant in
  let b = ((ax * py) - (ay * px)) / determinant in
  if (a * ax) + (b * bx) = px && (a * ay) + (b * by) = py then (a * 3) + b else 0

let () =
  lines
  |> List.sum (module Int) ~f:(fun (a, b, c) -> solve a b c)
  |> printf "Part 1: %d\n";
  lines
  |> List.sum
       (module Int)
       ~f:(fun (a, b, (px, py)) -> solve a b (px + 10000000000000, py + 10000000000000))
  |> printf "Part 2: %d\n"
