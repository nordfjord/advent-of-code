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
  points
  |> Hashtbl.to_alist
  |> List.sort ~compare:(fun (x0, _) (x1, _) -> Int.compare x0 x1)
  |> List.map ~f:snd
  |> function
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith "bad"

let mean fa =
  let open Float in
  Array.reduce_exn fa ~f:( + ) / of_int (Array.length fa)

let () =
  (* this is only possible because we start in the center of a square grid
     and the edges of the grid are all open.
     Futher, the fact that 26501365 is 202300 * 131 + 65 is also important
     This math wouldn't math in a lot of other circumstances
  *)
  let resolve (x, y) =
    match grid.(x % rows).(y % cols) with
    | '.' | 'S' -> Some (x, y)
    | _ -> None
  in
  let f0, f1, f2 = solve resolve start in
  (*
     the polynomial is of the form:
     f(x) = ax^2 + bx + c
     we know the value of f(0), f(1), and f(2)
     as such we can solve
     c = f(0),
     a = (f(2) + c - (2 * f(1))) / 2,
     b = f(1) - c - a
  *)
  let c = f0 in
  let a = (f2 + (c - (2 * f1))) / 2 in
  let b = f1 - c - a in
  let f x = (a * x * x) + (b * x) + c in
  let x = (26501365 - 65) / 131 in
  printf "f(x) = %dx^2 + %dx + %d\n" a b c;
  printf "f(%d) = %d\n" x (f x)
