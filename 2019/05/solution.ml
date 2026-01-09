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
  | Halt

let parse_op program i =
  let op = program.(i) in
  let operation = op mod 100 in
  let positional i = program.(program.(i)) in
  let immediate i = program.(i) in
  let param_1 = if op / 100 mod 10 = 1 then immediate else positional in
  let param_2 = if op / 1000 mod 10 = 1 then immediate else positional in
  match operation with
  | 1 -> Add (param_1 (i + 1), param_2 (i + 2), program.(i + 3))
  | 2 -> Mul (param_1 (i + 1), param_2 (i + 2), program.(i + 3))
  | 3 -> Store program.(i + 1)
  | 4 -> Output (param_1 (i + 1))
  | 5 -> JmpIfTrue (param_1 (i + 1), param_2 (i + 2))
  | 6 -> JmpIfFalse (param_1 (i + 1), param_2 (i + 2))
  | 7 -> LessThan (param_1 (i + 1), param_2 (i + 2), program.(i + 3))
  | 8 -> Equals (param_1 (i + 1), param_2 (i + 2), program.(i + 3))
  | 99 -> Halt
  | _ -> failwith ("Invalid opcode " ^ string_of_int program.(i))

let rec run_program input output program i =
  let op = parse_op program i in
  match op with
  | Add (a, b, dst) ->
    program.(dst) <- a + b;
    run_program input output program (i + 4)
  | Mul (a, b, dst) ->
    program.(dst) <- a * b;
    run_program input output program (i + 4)
  | Store dst ->
    program.(dst) <- input ();
    run_program input output program (i + 2)
  | Output value ->
    output value;
    run_program input output program (i + 2)
  | JmpIfTrue (a, b) ->
    if a <> 0
    then run_program input output program b
    else run_program input output program (i + 3)
  | JmpIfFalse (a, b) ->
    if a = 0
    then run_program input output program b
    else run_program input output program (i + 3)
  | LessThan (a, b, dst) ->
    program.(dst) <- (if a < b then 1 else 0);
    run_program input output program (i + 4)
  | Equals (a, b, dst) ->
    program.(dst) <- (if a = b then 1 else 0);
    run_program input output program (i + 4)
  | Halt -> program.(0)

let get_program () =
  let open Stdio in
  In_channel.input_line stdin
  |> Option.get
  |> String.split_on_char ','
  |> List.map int_of_string
  |> Array.of_list

let part1 () =
  let program = get_program () in
  let input () = 1 in
  let output = printf "Output: %d\n" in
  run_program input output program 0 |> printf "Part 1: %d\n"

let part2 () =
  let program = get_program () in
  let input () = 5 in
  let output = printf "Output: %d\n" in
  run_program input output program 0 |> printf "Part 2: %d\n"

let () =
  part1 ();
  part2 ()
