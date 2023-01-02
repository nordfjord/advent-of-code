open Printf

type operation =
  | Add of int * int * int
  | Mul of int * int * int
  | Store of int
  | Output of int
  | JmpIfTrue of int * int
  | JmpIfFalse of int * int
  | LessThan of int * int * int
  | Equals of int * int * int
  | SetRelativeBase of int
  | Halt

type computer =
  { mem : (int, int) Hashtbl.t
  ; mutable ip : int
  ; mutable relative_base : int
  }

let rec ( ** ) a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = a ** (n / 2) in
    b * b * if n mod 2 = 0 then 1 else a

let get computer i =
  match Hashtbl.find computer.mem i with
  | x -> x
  | exception Not_found -> 0

let set computer i v = Hashtbl.replace computer.mem i v
let get_op computer = get computer computer.ip

let param comp i =
  let op = get_op comp in
  let mode = op / (10 ** (i + 1)) mod 10 in
  match mode with
  | 1 -> comp.ip + i |> get comp
  | 2 -> comp.ip + i |> get comp |> ( + ) comp.relative_base |> get comp
  | _ -> comp.ip + i |> get comp |> get comp

let dst_param comp i =
  let op = get_op comp in
  let mode = op / (10 ** (i + 1)) mod 10 in
  match mode with
  | 2 -> get comp (comp.ip + i) + comp.relative_base
  | _ -> get comp (comp.ip + i)

let parse_op computer =
  let op = get_op computer in
  let operation = op mod 100 in
  let p = param computer in
  let dp = dst_param computer in
  match operation with
  | 1 -> Add (p 1, p 2, dp 3)
  | 2 -> Mul (p 1, p 2, dp 3)
  | 3 -> Store (dp 1)
  | 4 -> Output (p 1)
  | 5 -> JmpIfTrue (p 1, p 2)
  | 6 -> JmpIfFalse (p 1, p 2)
  | 7 -> LessThan (p 1, p 2, dp 3)
  | 8 -> Equals (p 1, p 2, dp 3)
  | 9 -> SetRelativeBase (p 1)
  | 99 -> Halt
  | _ -> failwith ("Invalid opcode " ^ string_of_int op)

type run_result =
  | InputRequested of (int -> run_result)
  | Out of (int * (unit -> run_result))
  | Halted

let rec run comp =
  let op = parse_op comp in
  match op with
  | Add (a, b, dst) ->
    set comp dst (a + b);
    comp.ip <- comp.ip + 4;
    run comp
  | Mul (a, b, dst) ->
    set comp dst (a * b);
    comp.ip <- comp.ip + 4;
    run comp
  | Store dst ->
    InputRequested
      (fun inp ->
        set comp dst inp;
        comp.ip <- comp.ip + 2;
        run comp)
  | Output value ->
    Out
      ( value
      , fun () ->
          comp.ip <- comp.ip + 2;
          run comp )
  | JmpIfTrue (a, b) ->
    comp.ip <- (if a <> 0 then b else comp.ip + 3);
    run comp
  | JmpIfFalse (a, b) ->
    comp.ip <- (if a = 0 then b else comp.ip + 3);
    run comp
  | LessThan (a, b, dst) ->
    set comp dst (if a < b then 1 else 0);
    comp.ip <- comp.ip + 4;
    run comp
  | Equals (a, b, dst) ->
    set comp dst (if a = b then 1 else 0);
    comp.ip <- comp.ip + 4;
    run comp
  | SetRelativeBase base ->
    comp.relative_base <- comp.relative_base + base;
    comp.ip <- comp.ip + 2;
    run comp
  | Halt -> Halted

let input = read_line ()

let get_computer () =
  let program =
    input |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  in
  let mem = Hashtbl.create (Array.length program) in
  for i = 0 to Array.length program - 1 do
    Hashtbl.add mem i program.(i)
  done;
  { mem; ip = 0; relative_base = 0 }

let get_program () = run (get_computer ())

let instr_seq program input =
  let p = ref program in
  let rec get_next = function
    | Halted -> None
    | InputRequested f ->
      (match input () with
       | None -> failwith "Input expected"
       | Some i -> get_next (f i))
    | Out (value, next) ->
      p := next ();
      Some value
  in
  Seq.of_dispenser (fun () -> get_next !p)

let rec chunk size (seq : 'a Seq.t) () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) -> Seq.Cons (Seq.cons x (Seq.take (size - 1) xs), chunk size seq)

let parse_instr = function
  | [ x; y; tile ] -> (x, y, tile)
  | _ -> failwith "Unexpected"

let part1 () =
  let floor = Hashtbl.create 300 in
  instr_seq (get_program ()) (fun () -> None)
  |> chunk 3
  |> Seq.map List.of_seq
  |> Seq.map parse_instr
  |> Seq.iter (fun (x, y, tile) -> Hashtbl.replace floor (x, y) tile);
  floor
  |> Hashtbl.to_seq
  |> Seq.filter (fun (_, t) -> t = 2)
  |> Seq.length
  |> printf "Part 1: %d\n"

let print tbl =
  for i = 0 to 26 do
    for j = 0 to 80 do
      match Hashtbl.find_opt tbl (j, i) with
      | Some 1 when i = 0 -> print_char '_'
      | Some 1 -> print_char '|'
      | Some 2 -> print_char '#'
      | Some 3 -> print_char '_'
      | Some 4 -> print_char 'o'
      | None | Some _ -> print_char ' '
    done;
    print_char '\n'
  done

let part2 () =
  let floor = Hashtbl.create 300 in
  let computer = get_computer () in
  Hashtbl.replace computer.mem 0 2;
  let score = ref 0 in
  let ball_and_paddle_location () =
    Hashtbl.to_seq floor
    |> Seq.filter (fun (_, tile) -> tile = 4 || tile = 3)
    |> List.of_seq
    |> function
    | [ a; b ] ->
      (match (a, b) with
       | ((ax, _), 4), ((bx, _), 3) -> (ax, bx)
       | ((ax, _), 3), ((bx, _), 4) -> (bx, ax)
       | _ -> raise Not_found)
    | _ -> raise Not_found
  in
  let program = run computer in
  let inputs =
    Seq.ints 0
    |> Seq.map (fun _ ->
         let ball_x, player_x = ball_and_paddle_location () in
         print_string "\027[2J";
         print floor;
         printf "Score: %d\n%!" !score;
         compare ball_x player_x)
    |> Seq.to_dispenser
  in
  instr_seq program inputs
  |> chunk 3
  |> Seq.map List.of_seq
  |> Seq.map parse_instr
  |> Seq.iter (fun (x, y, tile) ->
       match (x, y) with
       | -1, 0 -> score := tile
       | _ -> Hashtbl.replace floor (x, y) tile);
  print_string "\027[2J";
  print floor;
  printf "Score: %d\n%!" !score

let () =
  part1 ();
  part2 ()
