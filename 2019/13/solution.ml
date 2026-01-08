open Base
open Stdio

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
  { mutable mem : int array
  ; mutable ip : int
  ; mutable relative_base : int
  }

let get computer i = if i < Array.length computer.mem then computer.mem.(i) else 0

let set computer i v =
  if i >= Array.length computer.mem
  then (
    let new_mem = Array.create ~len:(i + 1) 0 in
    Array.blit
      ~src:computer.mem
      ~src_pos:0
      ~dst:new_mem
      ~dst_pos:0
      ~len:(Array.length computer.mem);
    computer.mem <- new_mem);
  computer.mem.(i) <- v

let get_op computer = get computer computer.ip

let param comp i =
  let op = get_op comp in
  let mode = op / (10 ** (i + 1)) % 10 in
  match mode with
  | 1 -> comp.ip + i |> get comp
  | 2 -> comp.ip + i |> get comp |> ( + ) comp.relative_base |> get comp
  | _ -> comp.ip + i |> get comp |> get comp

let dst_param comp i =
  let op = get_op comp in
  let mode = op / (10 ** (i + 1)) % 10 in
  match mode with
  | 2 -> get comp (comp.ip + i) + comp.relative_base
  | _ -> get comp (comp.ip + i)

let parse_op computer =
  let op = get_op computer in
  let operation = op % 100 in
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
  | _ -> failwith ("Invalid opcode " ^ Int.to_string op)

type run_result =
  | Input of (int -> unit)
  | Out of int
  | Halted

let rec run comp =
  match parse_op comp with
  | Add (a, b, dst) ->
    set comp dst (a + b);
    comp.ip <- comp.ip + 4;
    run comp
  | Mul (a, b, dst) ->
    set comp dst (a * b);
    comp.ip <- comp.ip + 4;
    run comp
  | Store dst ->
    Input
      (fun inp ->
        set comp dst inp;
        comp.ip <- comp.ip + 2)
  | Output value ->
    comp.ip <- comp.ip + 2;
    Out value
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

let input = In_channel.input_line stdin |> Option.value_exn

let get_computer () =
  let program =
    String.split input ~on:',' |> List.map ~f:Int.of_string |> Array.of_list
  in
  { mem = program; ip = 0; relative_base = 0 }

type instr =
  | DrawBlock of int * int
  | DrawBall of int * int
  | DrawPaddle of int * int
  | DrawWall of int * int
  | DrawEmpty of int * int
  | DrawScore of int
  | SendInput of (int -> unit)

let parse_instr = function
  | -1, 0, score -> DrawScore score
  | x, y, 0 -> DrawEmpty (x, y)
  | x, y, 1 -> DrawWall (x, y)
  | x, y, 2 -> DrawBlock (x, y)
  | x, y, 3 -> DrawPaddle (x, y)
  | x, y, 4 -> DrawBall (x, y)
  | x, y, _ -> DrawEmpty (x, y)

let next_instr computer =
  let rec aux acc comp =
    match run comp with
    | Halted -> None
    | Input f -> Some (SendInput f)
    | Out value ->
      (match acc with
       | [] -> aux [ value ] comp
       | [ a ] -> aux [ a; value ] comp
       | [ a; b ] -> Some (parse_instr (a, b, value))
       | _ -> failwith "Invalid")
  in
  aux [] computer

let part1 () =
  let computer = get_computer () in
  let rec aux count =
    match next_instr computer with
    | Some (DrawBlock _) -> aux count + 1
    | None -> count
    | _ -> aux count
  in
  aux 0

let print tbl =
  Array.map tbl ~f:String.of_array |> String.concat_array ~sep:"\n" |> printf "%s\n"

let next_score s = function
  | DrawScore s -> s
  | _ -> s

let part2 () =
  let board = Array.init 26 ~f:(fun _ -> Array.create ~len:81 ' ') in
  let computer = get_computer () in
  set computer 0 2;
  let update_board = function
    | DrawBall (x, y) -> board.(y).(x) <- 'o'
    | DrawPaddle (x, y) -> board.(y).(x) <- '_'
    | DrawBlock (x, y) -> board.(y).(x) <- '#'
    | DrawWall (x, 0) -> board.(0).(x) <- '_'
    | DrawWall (x, y) -> board.(y).(x) <- '|'
    | DrawEmpty (x, y) -> board.(y).(x) <- ' '
    | _ -> ()
  in
  let rec aux ballx paddlex score =
    let handle_instr = function
      | SendInput f ->
        f (compare ballx paddlex);
        aux ballx paddlex score
      | DrawScore s -> aux ballx paddlex s
      | DrawPaddle (x, _) -> aux ballx x score
      | DrawBall (x, _) -> aux x paddlex score
      | _ -> aux ballx paddlex score
    in
    match next_instr computer with
    | None -> score
    | Some instr ->
      update_board instr;
      printf "\027[2J";
      print board;
      printf "Score: %d\n%!" (next_score score instr);
      handle_instr instr
  in
  aux 0 0 0

let () =
  let p1 = part1 () in
  let p2 = part2 () in
  printf "\nPart 1: %d\n" p1;
  printf "Part 2: %d\n" p2
