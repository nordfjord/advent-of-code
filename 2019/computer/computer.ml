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

let get_computer input =
  let program =
    input |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  in
  let mem = Hashtbl.create (Array.length program) in
  for i = 0 to Array.length program - 1 do
    Hashtbl.add mem i program.(i)
  done;
  { mem; ip = 0; relative_base = 0 }

let get_program input = run (get_computer input)
