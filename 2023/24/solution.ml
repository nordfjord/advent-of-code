open Base
open Stdio

module Point3D = struct
  type t = float * float * float [@@deriving compare, sexp, hash, equal]

  let add (x1, y1, z1) (x2, y2, z2) = Float.(x1 + x2, y1 + y2, z1 + z2)
  let to_2d (x, y, _) = (x, y)
  let sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
end

module Point2D = struct
  type t = float * float [@@deriving compare, sexp, hash, equal]

  let add (x1, y1) (x2, y2) = Float.(x1 + x2, y1 + y2)
  let ( + ) = add

  let ( > ) (x1, y1) (x2, y2) =
    if Float.(x1 > x2) then true else if Float.(x1 < x2) then false else Float.(y1 > y2)

  let sub (x1, y1) (x2, y2) = Float.(x1 - x2, y1 - y2)
  let of_3d (x, y, _) = (x, y)
  let cross_product (x1, y1) (x2, y2) = Float.((x1 * y2) - (y1 * x2))
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
  |> List.map ~f:Float.of_string
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
