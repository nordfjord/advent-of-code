open Base
open Stdio

let grid = In_channel.input_lines stdin |> List.map ~f:String.to_array |> List.to_array

let start =
  Array.find_mapi_exn grid ~f:(fun i row ->
    Array.find_mapi row ~f:(fun j c ->
      match c with
      | 'S' -> Some (i, j)
      | _ -> None))

let rows = Array.length grid
let cols = Array.length grid.(0)
let adjacent (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let solve resolve start =
  let points = Hashtbl.create (module Int) in
  Sequence.range 1 (65 + (rows * 2)) ~stop:`inclusive
  |> Sequence.fold ~init:(Hash_set.Poly.of_list [ start ]) ~f:(fun acc i ->
    let result = Hash_set.Poly.create ~size:(Hash_set.length acc * 3) () in
    Hash_set.iter acc ~f:(fun pos ->
      adjacent pos |> List.filter_map ~f:resolve |> List.iter ~f:(Hash_set.add result));
    if i = 64 then printf "part 1: %d\n%!" (Hash_set.length result);
    if i % rows = 65
    then (
      Hashtbl.set points ~key:(i / rows) ~data:(Hash_set.length result);
      printf "f(%d) = %d\n%!" (i / rows) (Hash_set.length result));
    result)
  |> ignore;
  points |> Hashtbl.to_alist

let mean fa =
  let open Float in
  Array.reduce_exn fa ~f:( + ) / of_int (Array.length fa)

let regression xs ys =
  let open Float in
  let xm = mean xs in
  let ym = mean ys in
  let x2m = Array.map xs ~f:(fun x -> x * x) |> mean in
  let x3m = Array.map xs ~f:(fun x -> x * x * x) |> mean in
  let x4m =
    Array.map xs ~f:(fun x ->
      let x2 = x * x in
      x2 * x2)
    |> mean
  in
  let xzipy = Array.zip_exn xs ys in
  let xym = Array.map xzipy ~f:(fun (x, y) -> x * y) |> mean in
  let x2ym = Array.map xzipy ~f:(fun (x, y) -> x * x * y) |> mean in
  let sxx = x2m - (xm * xm) in
  let sxy = xym - (xm * ym) in
  let sxx2 = x3m - (xm * x2m) in
  let sx2x2 = x4m - (x2m * x2m) in
  let sx2y = x2ym - (x2m * ym) in
  let b = ((sxy * sx2x2) - (sx2y * sxx2)) / ((sxx * sx2x2) - (sxx2 * sxx2)) in
  let c = ((sx2y * sxx) - (sxy * sxx2)) / ((sxx * sx2x2) - (sxx2 * sxx2)) in
  let a = ym - (b * xm) - (c * x2m) in
  let to_int f = Int.of_float (Float.round f) in
  (to_int a, to_int b, to_int c)

let () =
  (* this is only possible because we start in the center of a square grid
     and the edges of the grid are all open.
     Futher, the fact that 26501365 is 202300 * 131 + 65 is also important
     This math wouldn't math in a lot of other circumstances
  *)
  let x = (26501365 - 65) / 131 in
  let resolve (x, y) =
    match grid.(x % rows).(y % cols) with
    | '.' | 'S' -> Some (x, y)
    | _ -> None
  in
  let points = solve resolve start in
  let xs, ys = List.unzip points in
  let a, b, c =
    regression
      (Array.of_list @@ List.map xs ~f:Int.to_float)
      (Array.of_list @@ List.map ys ~f:Int.to_float)
  in
  let f x = a + (b * x) + (c * x * x) in
  printf "f(x) = %dx^2 + %dx + %dx\n" c b a;
  printf "f(%d) = %d\n" x (f x)
