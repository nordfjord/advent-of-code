open Printf
open Computer

let input =
  let fh = open_in "./input.txt" in
  let line = input_line fh in
  close_in fh;
  line

let rec to_seq p =
  let p = ref p in
  Seq.of_dispenser (fun () ->
    match !p with
    | Out (value, f) ->
      p := f ();
      Some value
    | Halted -> None
    | InputRequested _ -> failwith "Not expecting input requests")

let adjacent (i, j) = [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]

let () =
  print_char '\n';
  let program = get_program input in
  let map =
    program
    |> to_seq
    |> Seq.map Char.chr
    |> String.of_seq
    |> String.split_on_char '\n'
    |> Array.of_list
  in
  let crosses = Hashtbl.create 10 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to String.length map.(i) - 1 do
      let is_cross =
        map.(i).[j] = '#'
        && adjacent (i, j)
           |> List.for_all (fun (i, j) ->
             0 <= i
             && i <= Array.length map - 1
             && 0 <= j
             && j <= String.length map.(i) - 1
             && map.(i).[j] = '#')
      in
      if is_cross
      then (
        Hashtbl.add crosses (i, j) true;
        print_char 'X')
      else print_char map.(i).[j]
    done;
    print_char '\n'
  done;
  crosses
  |> Hashtbl.to_seq_keys
  |> Seq.map (fun (x, y) -> x * y)
  |> Seq.fold_left ( + ) 0
  |> printf "Part 1: %d\n"

(* Did this manually *)
let uncompressed_path =
  "L,10,R,8,R,8,L,10,R,8,R,8,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,R,10,L,12,R,10,L,10,R,8,R,8"

let path = "A,A,B,C,B,C,B,C,C,A"
let a = "L,10,R,8,R,8"
let b = "L,10,L,12,R,8,R,10"
let c = "R,10,L,12,R,10"

let input_seq =
  [ path; a; b; c; "n\n" ] |> String.concat "\n" |> String.to_seq |> Seq.map Char.code

let rec with_input_output program input output =
  match program with
  | InputRequested f -> with_input_output (f (input ())) input output
  | Out (value, f) ->
    output value;
    with_input_output (f ()) input output
  | Halted -> ()

let unsafe_dispenser disp () =
  match disp () with
  | Some x -> x
  | None -> raise Not_found

let () =
  print_char '\n';
  let computer = get_computer input in
  Hashtbl.replace computer.mem 0 2;
  let program = run computer in
  with_input_output
    program
    (unsafe_dispenser (Seq.to_dispenser input_seq))
    (fun c -> if c >= 128 then printf "Output: %d\n" c else print_char (Char.chr c))
