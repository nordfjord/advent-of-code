open Base

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

let get computer i =
  match Hashtbl.find computer.mem i with
  | Some x -> x
  | None -> 0

let set computer i v = Hashtbl.set computer.mem ~key:i ~data:v
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

let copy computer =
  let new_mem = Hashtbl.copy computer.mem in
  { mem = new_mem; ip = computer.ip; relative_base = computer.relative_base }

let get_computer input =
  let program =
    input |> String.split ~on:',' |> List.map ~f:Int.of_string |> Array.of_list
  in
  let mem = Hashtbl.create (module Int) ~size:(Array.length program) in
  for i = 0 to Array.length program - 1 do
    Hashtbl.set mem ~key:i ~data:program.(i)
  done;
  { mem; ip = 0; relative_base = 0 }

let get_program input = run (get_computer input)
