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

type run_result =
  | InputRequested of (int -> run_result)
  | Out of (int * (unit -> run_result))
  | Halted

let rec run program ip =
  let op = parse_op program ip in
  match op with
  | Add (a, b, dst) ->
    program.(dst) <- a + b;
    run program (ip + 4)
  | Mul (a, b, dst) ->
    program.(dst) <- a * b;
    run program (ip + 4)
  | Store dst ->
    InputRequested
      (fun inp ->
        program.(dst) <- inp;
        run program (ip + 2))
  | Output value -> Out (value, fun () -> run program (ip + 2))
  | JmpIfTrue (a, b) ->
    let ip = if a <> 0 then b else ip + 3 in
    run program ip
  | JmpIfFalse (a, b) ->
    let ip = if a = 0 then b else ip + 3 in
    run program ip
  | LessThan (a, b, dst) ->
    program.(dst) <- (if a < b then 1 else 0);
    run program (ip + 4)
  | Equals (a, b, dst) ->
    program.(dst) <- (if a = b then 1 else 0);
    run program (ip + 4)
  | Halt -> Halted

let get_program init =
  let program =
    Prelude.Aoc.stdin_seq ()
    |> List.of_seq
    |> List.hd
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list
  in
  match run program 0 with
  | InputRequested f -> f init
  | _ -> failwith "Unexpected program behaviour"

let to_dispenser l =
  let disp = Seq.to_dispenser (List.to_seq l) in
  fun () ->
    match disp () with
    | Some x -> x
    | None -> raise Not_found

let part1 () =
  let result = ref 0 in
  for a = 0 to 4 do
    for b = 0 to 4 do
      for c = 0 to 4 do
        for d = 0 to 4 do
          for e = 0 to 4 do
            if [ a; b; c; d; e ] |> List.sort_uniq compare |> List.length = 5
            then (
              let output =
                [ a; b; c; d; e ]
                |> List.map get_program
                |> List.fold_left
                     (fun out program ->
                       match program with
                       | InputRequested f ->
                         (match f out with
                          | Out (value, _) -> value
                          | Halted -> out
                          | _ -> failwith "unexpected program behaviour")
                       | Out (value, _) -> value
                       | Halted -> out)
                     0
              in
              result := max !result output)
          done
        done
      done
    done
  done;
  !result |> printf "Part 1: %d\n"

let () = part1 ()

(* Honestly f**k doing this in a language that doesn't permit forward referencing.
   I rewrote the int computer to support continuation passing to be able to do this
   crap
 *)
let rec run_until_output cell p =
  match p with
  | InputRequested f -> run_until_output cell (f !cell)
  | Out (value, p) ->
    cell := value;
    p ()
  | Halted -> Halted

let part2 () =
  let result = ref 0 in
  for a = 5 to 9 do
    for b = 5 to 9 do
      for c = 5 to 9 do
        for d = 5 to 9 do
          for e = 5 to 9 do
            if [ a; b; c; d; e ] |> List.sort_uniq compare |> List.length = 5
            then (
              let output = ref 0 in
              let programs = ref ([ a; b; c; d; e ] |> List.map get_program) in
              while !programs |> List.for_all (fun s -> s <> Halted) do
                programs := !programs |> List.map (run_until_output output)
              done;
              result := max !result !output)
          done
        done
      done
    done
  done;
  !result |> printf "Part 2: %d\n"

let () = part2 ()
