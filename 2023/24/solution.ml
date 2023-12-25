open Base
open Stdio

module Point3D = struct
  type t = int * int * int [@@deriving compare, sexp, hash, equal]

  let to_2d (x, y, _) = (Float.of_int x, Float.of_int y)
end

module Point2D = struct
  type t = float * float [@@deriving compare, sexp, hash, equal]

  let sub (x1, y1) (x2, y2) = Float.(x1 - x2, y1 - y2)

  let cross_product (x1, y1) (x2, y2) =
    let open Float in
    (x1 * y2) - (y1 * x2)
end

module Line = struct
  type t =
    { point : Point3D.t
    ; direction : Point3D.t
    }
  [@@deriving compare, sexp, hash, equal]

  let intersects_2d l1 l2 =
    let p1 = Point3D.to_2d l1.point in
    let p2 = Point3D.to_2d l2.point in
    let v1 = Point3D.to_2d l1.direction in
    let v2 = Point3D.to_2d l2.direction in
    let p = Point2D.sub p2 p1 in
    let d = Point2D.cross_product v1 v2 in
    if Float.equal d 0.0
    then None (* Lines are parallel, no intersection *)
    else (
      let t = Point2D.cross_product p v2 /. d in
      let s = Point2D.cross_product p v1 /. d in
      (* if the intersection is after the end of the line it's discarded *)
      if Float.(t < 0. || s < 0.)
      then None
      else (
        let x1, y1 = p1 in
        let dx, dy = v1 in
        let intersection = (x1 +. (t *. dx), y1 +. (t *. dy)) in
        Some intersection))
end

let parse_coordinates str =
  Str.split (Str.regexp_string ", ") str
  |> List.map ~f:String.strip
  |> List.map ~f:Int.of_string
  |> function
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "invalid coordinates"

let points =
  In_channel.input_lines stdin
  |> List.map ~f:(fun line ->
    match Str.split (Str.regexp " @ ") line with
    | [ left; right ] ->
      { Line.point = parse_coordinates left; direction = parse_coordinates right }
    | _ -> failwith "invalid line")

let intersections points =
  let rec loop points acc =
    match points with
    | [] -> acc
    | line :: rest ->
      let intersections = List.filter_map rest ~f:(Line.intersects_2d line) in
      loop rest (List.append acc intersections)
  in
  loop points []

let () =
  let intersections = intersections points in
  let area_min = 200000000000000. in
  let area_max = 400000000000000. in
  (*
     let area_min = 7. in
     let area_max = 27. in *)
  let counts =
    List.count intersections ~f:(fun (x, y) ->
      Float.(x >= area_min && x <= area_max && y >= area_min && y <= area_max))
  in
  printf "%d\n" counts

let () =
  let open Z3 in
  let ctx = mk_context [] in
  let ( + ) a b = Arithmetic.mk_add ctx [ a; b ] in
  let ( * ) a b = Arithmetic.mk_mul ctx [ a; b ] in
  let ( = ) a b = Boolean.mk_eq ctx a b in
  let of_int x = Arithmetic.Integer.mk_numeral_i ctx x in
  let mk_int name = Arithmetic.Integer.mk_const_s ctx name in
  let x = mk_int "x" in
  let y = mk_int "y" in
  let z = mk_int "z" in
  let vx = mk_int "vx" in
  let vy = mk_int "vy" in
  let vz = mk_int "vz" in
  let solver = Solver.mk_solver ctx None in
  List.iteri points ~f:(fun i { point = xi, yi, zi; direction = vxi, vyi, vzi } ->
    let t = mk_int (Printf.sprintf "t%d" i) in
    Solver.add
      solver
      [ x + (t * vx) = of_int xi + (t * of_int vxi)
      ; y + (t * vy) = of_int yi + (t * of_int vyi)
      ; z + (t * vz) = of_int zi + (t * of_int vzi)
      ]);
  let _ = Solver.check solver [] in
  let model = Solver.get_model solver |> Option.value_exn in
  let sum = Arithmetic.mk_add ctx [ x; y; z ] in
  let (Some sum) = Model.eval model sum true in
  let sum = Arithmetic.Integer.get_big_int sum in
  printf "%s\n" (Z.to_string sum)
[@@warning "-8"]
