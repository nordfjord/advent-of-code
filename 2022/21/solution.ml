open Printf

let input =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None )
  |> Array.of_seq

type action = Yell of int | Math of string * string * string

let monkeys =
  input
  |> Array.map (fun line ->
         try Scanf.sscanf line "%s@: %d" (fun monkey n -> (monkey, Yell n))
         with Scanf.Scan_failure _ ->
           Scanf.sscanf line "%s@: %s %s %s" (fun monkey name1 op name2 ->
               ( monkey
               , Math
                   ( name1
                   , name2
                   , match op with
                     | "+" ->
                         op
                     | "-" ->
                         op
                     | "*" ->
                         op
                     | "/" ->
                         op
                     | _ ->
                         failwith ("Op " ^ op ^ "not recognized") ) ) ) )

let tbl =
  let tbl = Hashtbl.create 15 in
  monkeys |> Array.iter (fun (m, v) -> Hashtbl.add tbl m v) ;
  tbl

let rec calc action =
  match action with
  | Yell n ->
      n
  | Math (name1, name2, op) -> (
      let m1 = Hashtbl.find tbl name1 in
      let m2 = Hashtbl.find tbl name2 in
      match op with
      | "+" ->
          calc m1 + calc m2
      | "-" ->
          calc m1 - calc m2
      | "*" ->
          calc m1 * calc m2
      | "/" ->
          calc m1 / calc m2
      | _ ->
          failwith "WTF" )

let part1 () = calc (Hashtbl.find tbl "root") |> printf "Part 1: %d\n"

let () = part1 ()

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Const of int
  | X

let rec make_tree name =
  let node = Hashtbl.find tbl name in
  match node with
  | Yell _ when name = "humn" ->
      X
  | Yell n ->
      Const n
  | Math (n1, n2, op) -> (
    match op with
    | "+" ->
        Add (make_tree n1, make_tree n2)
    | "-" ->
        Sub (make_tree n1, make_tree n2)
    | "*" ->
        Mult (make_tree n1, make_tree n2)
    | "/" ->
        Div (make_tree n1, make_tree n2)
    | _ ->
        failwith "WTF" )

let rec eval expr =
  match expr with
  | Const n ->
      n
  | X ->
      raise Not_found
  | Add (a, b) ->
      eval a + eval b
  | Sub (a, b) ->
      eval a - eval b
  | Mult (a, b) ->
      eval a * eval b
  | Div (a, b) ->
      eval a / eval b

(* Recursively go through the tree, evaluating the parts we can
   each evaluation applies its inverse to the target such
   that by the time we get to the X expression the remainder is
   the solution *)
let rec solve expr target =
  match expr with
  | Const n ->
      n
  | Add (a, b) -> (
    match eval a with
    | n ->
        solve b (target - n)
    | exception Not_found ->
        solve a (target - eval b) )
  | Sub (a, b) -> (
    match eval a with
    | n ->
        solve b (n - target)
    | exception Not_found ->
        solve a (eval b + target) )
  | Mult (a, b) -> (
    match eval a with
    | n ->
        solve b (target / n)
    | exception Not_found ->
        solve a (target / eval b) )
  | Div (a, b) -> (
    match eval a with
    | n ->
        solve b (n / target)
    | exception Not_found ->
        solve a (target * eval b) )
  | X ->
      target

let part2_smart () =
  let expr1, expr2 =
    match Hashtbl.find tbl "root" with
    | Math (n1, n2, _) ->
        (make_tree n1, make_tree n2)
    | Yell _ ->
        failwith "WTF"
  in
  let target, formula =
    match eval expr1 with
    | target ->
        (target, expr2)
    | exception Not_found ->
        (eval expr2, expr1)
  in
  printf "Part 2 (smart): %d\n" (solve formula target)

let () = part2_smart ()
