open Prelude
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
    set comp dst (a + b)
    ; comp.ip <- comp.ip + 4
    ; run comp
  | Mul (a, b, dst) ->
    set comp dst (a * b)
    ; comp.ip <- comp.ip + 4
    ; run comp
  | Store dst ->
    InputRequested
      (fun inp ->
        set comp dst inp
        ; comp.ip <- comp.ip + 2
        ; run comp)
  | Output value ->
    Out
      ( value
      , fun () ->
          comp.ip <- comp.ip + 2
          ; run comp )
  | JmpIfTrue (a, b) ->
    comp.ip <- (if a <> 0 then b else comp.ip + 3)
    ; run comp
  | JmpIfFalse (a, b) ->
    comp.ip <- (if a = 0 then b else comp.ip + 3)
    ; run comp
  | LessThan (a, b, dst) ->
    set comp dst (if a < b then 1 else 0)
    ; comp.ip <- comp.ip + 4
    ; run comp
  | Equals (a, b, dst) ->
    set comp dst (if a = b then 1 else 0)
    ; comp.ip <- comp.ip + 4
    ; run comp
  | SetRelativeBase base ->
    comp.relative_base <- comp.relative_base + base
    ; comp.ip <- comp.ip + 2
    ; run comp
  | Halt -> Halted

let input =
  let fh = open_in "./input.txt" in
  let line = input_line fh in
  close_in fh
  ; line

let get_computer () =
  let program =
    input |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  in
  let mem = Hashtbl.create (Array.length program) in
  for i = 0 to Array.length program - 1 do
    Hashtbl.add mem i program.(i)
  done
  ; { mem; ip = 0; relative_base = 0 }

let get_program () = run (get_computer ())

let robot program =
  let p = ref program in
  let rec get_next value =
    match !p with
    | Halted -> 99
    | InputRequested f ->
      (match value with
       | None -> failwith "Input expected"
       | Some i ->
         p := f i
         ; get_next None)
    | Out (value, next) ->
      p := next ()
      ; value
  in
  fun inp -> get_next (Some inp)

let rec chunk size (seq : 'a Seq.t) () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) -> Seq.Cons (Seq.cons x (Seq.take (size - 1) xs), chunk size seq)

let up = 1, 0
let down = -1, 0
let left = 0, -1
let right = 0, 1

let direction_vector = function
  | 1 -> up
  | 2 -> down
  | 3 -> left
  | 4 -> right
  | _ -> failwith "Invalid"

type tile =
  | Unknown
  | Floor
  | Wall
  | OxygenSystem
[@@deriving show]

let get_tile m t = Hashtbl.find_opt m t |> Option.value ~default:Unknown

let possible_moves map loc =
  [ 1; 2; 3; 4 ]
  |> List.filter (fun l ->
       let l = Point.(loc + direction_vector l) in
       match get_tile map l with
       | Unknown | OxygenSystem | Floor -> true
       | _ -> false)

let path_to_location path =
  path |> List.fold_left (fun loc d -> Point.(loc + direction_vector d)) (0, 0)

let bfs map =
  let q = Queue.create () in
  q |> Queue.add []
  ; Hashtbl.add map (0, 0) Floor
  ; let result = ref [] in
    while not (Queue.is_empty q) do
      let path = Queue.take q in
      let location = path_to_location path in
      let tile = get_tile map location in
      if tile = OxygenSystem
      then result := path
      else
        [ 1; 2; 3; 4 ]
        |> List.iter (fun d ->
             let robot = robot (get_program ()) in
             path |> List.iter (fun d -> robot d |> ignore)
             ; let result = robot d in
               let loc = Point.(location + direction_vector d) in
               if Hashtbl.mem map loc
               then ()
               else (
                 match result with
                 | 0 -> Hashtbl.replace map loc Wall
                 | 1 ->
                   Hashtbl.replace map loc Floor
                   ; Queue.add (path @ [ d ]) q
                 | 2 ->
                   Hashtbl.replace map loc OxygenSystem
                   ; Queue.add (path @ [ d ]) q
                 | _ -> failwith "Unexpected output"))
    done
    ; !result

let read () =
  printf "Input: "
  ; read_int ()

let manual () =
  let robot = robot (get_program ()) in
  let inp = ref (read ()) in
  while !inp <> 0 do
    printf "Output: %d\n" (robot !inp)
    ; inp := read ()
  done

let part1 () =
  let map = Hashtbl.create 3000 in
  let result = bfs map in
  printf "Part 1: %d\n" (List.length result)
  ; for i = -19 to 21 do
      for j = -21 to 19 do
        match Hashtbl.find_opt map (i, j) with
        | Some Wall -> print_char '#'
        | Some Floor when (i, j) = (0, 0) -> print_char 'S'
        | Some Floor -> print_char '.'
        | Some OxygenSystem -> print_char 'O'
        | None | Some Unknown -> print_char '?'
      done
      ; print_char '\n'
    done
  ; map

let bfs map start =
  let q = Queue.create () in
  q |> Queue.add (start, 0)
  ; let visited = Hashtbl.create 3000 in
    Hashtbl.add visited start ()
    ; let result = ref 0 in
      while not (Queue.is_empty q) do
        let location, minutes = Queue.take q in
        result := max minutes !result
        ; [ up; down; left; right ]
          |> List.iter (fun d ->
               let loc = Point.(location + d) in
               if Hashtbl.mem visited loc
               then ()
               else (
                 match get_tile map loc with
                 | Wall | Unknown -> ()
                 | Floor | OxygenSystem ->
                   Hashtbl.add visited loc ()
                   ; Queue.add (loc, minutes + 1) q))
      done
      ; !result

let part2 map =
  let oxygen_loc =
    Hashtbl.to_seq map
    |> Seq.find_map (fun (k, v) -> if v = OxygenSystem then Some k else None)
    |> Option.get
  in
  bfs map oxygen_loc |> printf "Part 2: %d\n"

let () = part1 () |> part2
