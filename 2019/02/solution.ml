open Printf

type operation = 
  | Add of int * int * int
  | Mul of int * int * int
  | Halt

let get_op program i =
  match program.(i) with
  | 1 -> Add(program.(i + 1), program.(i + 2), program.(i + 3))
  | 2 -> Mul(program.(i + 1), program.(i + 2), program.(i + 3))
  | 99 -> Halt
  | _ -> failwith ("Invalid opcode " ^ (string_of_int program.(i)) )

let rec run_program program i =
  match get_op program i with
  | Add(a,b, dst) -> 
      program.(dst) <- program.(a) + program.(b);
      run_program program (i + 4)
  | Mul(a, b, dst) -> 
      program.(dst) <- program.(a) * program.(b);
      run_program program (i + 4)
  | Halt -> program.(0)

let get_program() = Prelude.Aoc.stdin_seq() |> List.of_seq |> List.hd |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list

let part1() =
  let program = get_program() in 
  program.(1) <- 12;
  program.(2) <- 2;
  run_program program 0
  |> printf "Part 1: %d\n"

let rec find_target program target i j =
  if i > 99 then find_target program target 0 (j + 1)
  else if j > 99 then failwith "No solution found" 
  else 
    (let p = Array.copy program in
    p.(1) <- i; p.(2)  <- j;
    if run_program p 0 = target then 100 * i + j
    else find_target program target (i + 1) j
    )



let part2() =
  let program = get_program() in 
  let target = 19690720 in
  find_target program target 0 0
  |> printf "Part 2: %d\n"
  
     


let () = part1(); part2()
