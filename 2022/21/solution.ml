open Printf

let input =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None)
  |> Array.of_seq

type action = Yell of int | Math of string * string * string

let monkeys =
  input
  |> Array.map (fun line ->
         try Scanf.sscanf line "%s@: %d" (fun monkey n -> (monkey, Yell n))
         with Scanf.Scan_failure _ ->
           Scanf.sscanf line "%s@: %s %s %s" (fun monkey name1 op name2 ->
               ( monkey,
                 Math
                   ( name1,
                     name2,
                     match op with
                     | "+" -> op
                     | "-" -> op
                     | "*" -> op
                     | "/" -> op
                     | _ -> failwith ("Op " ^ op ^ "not recognized") ) )))

let tbl =
  let tbl = Hashtbl.create 15 in
  monkeys |> Array.iter (fun (m, v) -> Hashtbl.add tbl m v);
  tbl

let rec calc action =
  match action with
  | Yell n -> n
  | Math (name1, name2, op) -> (
      let m1 = Hashtbl.find tbl name1 in
      let m2 = Hashtbl.find tbl name2 in
      match op with
      | "+" -> calc m1 + calc m2
      | "-" -> calc m1 - calc m2
      | "*" -> calc m1 * calc m2
      | "/" -> calc m1 / calc m2
      | _ -> failwith "WTF")

let rec part1 () = calc (Hashtbl.find tbl "root") |> printf "Part 1: %d\n"

type math_expr =
  | Add of math_expr * math_expr
  | Sub of math_expr * math_expr
  | Mult of math_expr * math_expr
  | Div of math_expr * math_expr
  | Const of int
  | X
[@@deriving show]

let rec math_tree name =
  let node = Hashtbl.find tbl name in
  match node with
  | Yell _ when name = "humn" -> X
  | Yell n -> Const n
  | Math (n1, n2, op) -> (
      match op with
      | "+" -> Add (math_tree n1, math_tree n2)
      | "-" -> Sub (math_tree n1, math_tree n2)
      | "*" -> Mult (math_tree n1, math_tree n2)
      | "/" -> Div (math_tree n1, math_tree n2)
      | _ -> failwith "WTF")

let rec render_expr expr =
  match expr with
  | Const n -> string_of_int n
  | X -> "x"
  | Add (a, b) -> sprintf "(%s + %s)" (render_expr a) (render_expr b)
  | Sub (a, b) -> sprintf "(%s - %s)" (render_expr a) (render_expr b)
  | Div (a, b) -> sprintf "(%s / %s)" (render_expr a) (render_expr b)
  | Mult (a, b) -> sprintf "(%s * %s)" (render_expr a) (render_expr b)

let rec eval_expr expr x =
  match expr with
  | Const n -> n
  | X -> x
  | Add (a, b) -> eval_expr a x + eval_expr b x
  | Sub (a, b) -> eval_expr a x - eval_expr b x
  | Mult (a, b) -> eval_expr a x * eval_expr b x
  | Div (a, b) -> eval_expr a x / eval_expr b x

let rec simplify expr =
  match expr with
  | Const n -> Const n
  | X -> X
  | Add (a, b) -> (
      match (simplify a, simplify b) with
      | Const a, Const b -> Const (a + b)
      (* addition is associative *)
      | Const a, Add (Const b, d) -> simplify (Add (Const (a + b), d))
      | Const a, Add (b, Const d) -> simplify (Add (Const (a + d), b))
      | Const 0, b -> b
      | a, Const 0 -> a
      | a, b -> Add (a, b))
  | Sub (a, b) -> (
      match (simplify a, simplify b) with
      | Const a, Const b -> Const (a - b)
      | a, Const 0 -> a
      | a, b -> Sub (a, b))
  | Mult (a, b) -> (
      match (simplify a, simplify b) with
      | Const a, Const b -> Const (a * b)
      (* mult is associative *)
      | Const a, Mult (Const b, d) -> simplify (Mult (Const (a * b), d))
      | Const 1, b -> b
      | a, Const 1 -> a
      | a, b -> Mult (a, b))
  | Div (a, b) -> (
      match (simplify a, simplify b) with
      | Const a, Const b -> Const (a / b)
      | a, Const 1 -> a
      | a, b -> Div (a, b))

let rec has_human expr =
  match expr with
  | X -> true
  | Const _ -> false
  | Add (a, b) -> has_human a || has_human b
  | Sub (a, b) -> has_human a || has_human b
  | Mult (a, b) -> has_human a || has_human b
  | Div (a, b) -> has_human a || has_human b

let rec binary_search cmp f x lo hi =
  if lo > hi then raise Not_found
  else
    let mid = (lo + hi) / 2 in
    let value = f mid in
    printf "Testing x=%d; f(x)=%d\n" mid value;
    if value = x then mid
    else if cmp value x then binary_search cmp f x (mid + 1) hi
    else binary_search cmp f x lo (mid - 1)

let rec part2 () =
  let human_expr, monkeys_expr =
    match Hashtbl.find tbl "root" with
    | Math (n1, n2, _) -> (math_tree n1, math_tree n2)
    | Yell _ -> failwith "WTF"
  in
  let simplified = simplify human_expr in
  printf "expr1=%s\n" (render_expr simplified);
  printf "expr2=%s\n" (render_expr (simplify monkeys_expr));
  printf "expr2=%s\n" (show_math_expr simplified);
  let solution = eval_expr monkeys_expr 0 in
  printf "Attempting to find solution for human that ends up with %d\n" solution;

  let max_range = 1_000_000_000_000_000 in
  let min_range = -max_range in

  let x =
    try
      binary_search ( > ) (eval_expr simplified) solution min_range max_range
    with Not_found ->
      binary_search ( < ) (eval_expr simplified) solution min_range max_range
  in
  printf "Part 2: %d\n" x

let () =
  part1 ();
  part2 ()
