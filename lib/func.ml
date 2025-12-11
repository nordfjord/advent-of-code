open Base

let memo f =
  let tbl = Hashtbl.Poly.create () in
  fun x ->
    match Hashtbl.find tbl x with
    | Some y -> y
    | None ->
      let y = f x in
      Hashtbl.set tbl ~key:x ~data:y;
      y

let memo_rec f =
  let tbl = Hashtbl.Poly.create () in
  let rec go x =
    match Hashtbl.find tbl x with
    | Some y -> y
    | None ->
      let y = f go x in
      Hashtbl.set tbl ~key:x ~data:y;
      y
  in
  go
