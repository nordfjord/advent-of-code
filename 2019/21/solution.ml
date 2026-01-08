open Base
open Stdio

let start_timer = Time_now.nanosecond_counter_for_timing ()
let line = In_channel.with_file "./input.txt" ~f:In_channel.input_line |> Option.value_exn
let computer = Computer.get_computer line

module SpringScript = struct
  type t =
    | And of char * char
    | Or of char * char
    | Not of char * char
    | Walk
    | Run
  [@@deriving sexp_of]

  let to_string = function
    | And (a, b) -> Printf.sprintf "AND %c %c" a b
    | Or (a, b) -> Printf.sprintf "OR %c %c" a b
    | Not (a, b) -> Printf.sprintf "NOT %c %c" a b
    | Walk -> "WALK"
    | Run -> "RUN"

  let to_string instructions =
    instructions
    |> List.map ~f:to_string
    |> String.concat ~sep:"\n"
    |> Fn.flip String.append "\n"
end

let run_program comp program =
  let rec aux i = function
    | Computer.Halted -> 0
    | Computer.InputRequested f ->
      Out_channel.flush stdout;
      let ascii = Char.to_int program.[i] in
      let next_comp = f ascii in
      aux (i + 1) next_comp
    | Computer.Out (value, f) ->
      (match Char.of_int value with
       | Some c ->
         Out_channel.output_char stdout c;
         let next_comp = f () in
         aux i next_comp
       | None -> value)
  in
  aux 0 (Computer.run comp)

let part1 () =
  let computer = Computer.copy computer in
  SpringScript.to_string
    [ Not ('B', 'J') (* if B is hole, jump *)
    ; Not ('C', 'T') (* if C is hole, T=true *)
    ; Or ('T', 'J') (* J=true if B or C is hole *)
    ; And ('D', 'J') (* only jump if D is ground *)
    ; Not ('A', 'T') (* if A is a hole, T=true *)
    ; Or ('T', 'J') (* J=true if A is hole or B/C is hole *)
    ; Walk
    ]
  |> run_program computer

let part2 () =
  let computer = Computer.copy computer in
  (* MUST NOT JUMP IF H IS A HOLE
    .................
    .................
    ..@..............
    #####.#.##.#.####
       ABCDEFGHI
       110101101 *)
  SpringScript.to_string
    [ Not ('B', 'J') (* if B is hole, jump *)
    ; Not ('C', 'T') (* if C is hole, T=true *)
    ; Or ('T', 'J') (* J=true if B or C is hole *)
    ; And ('D', 'J') (* only jump if D is ground *)
    ; And ('H', 'J') (* only jump if H is ground *)
    ; Not ('A', 'T') (* if A is a hole, T=true, we must jump otherwise we fall *)
    ; Or ('T', 'J') (* J=true if A is hole or B/C is hole *)
    ; Run
    ]
  |> run_program computer

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf
    "Execution time: %.3f ms\n"
    (Int63.to_float Int63.(stop - start_timer) /. 1_000_000.)
