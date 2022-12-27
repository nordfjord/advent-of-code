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

let part1 () = calc (Hashtbl.find tbl "root") |> printf "Part 1: %d\n"

let rec binary_search cmp f x lo hi =
  if lo > hi then raise Not_found
  else
    let mid = (lo + hi) / 2 in
    let value = f mid in
    if value = x then mid
    else if cmp value x then binary_search cmp f x (mid + 1) hi
    else binary_search cmp f x lo (mid - 1)

let part2 () =
  let human_expr, monkeys_expr =
    match Hashtbl.find tbl "root" with
    | Math (n1, n2, _) -> (Hashtbl.find tbl n1, Hashtbl.find tbl n2)
    | Yell _ -> failwith "WTF"
  in
  let solution = calc monkeys_expr in

  let calc x =
    Hashtbl.replace tbl "humn" (Yell x);
    calc human_expr
  in

  let max_range = 1_000_000_000_000_000 in
  let min_range = 0 in

  let cmp = if calc 0 < calc 10000 then ( < ) else ( > ) in

  let x = binary_search cmp calc solution min_range max_range in
  printf "Part 2: %d\n" x

let () =
  part1 ();
  part2 ()
