type t = {
  operation : int -> int;
  testDivisor : int;
  whenTrue : int;
  whenFalse : int;
  mutable items : int list;
  mutable inspectCount : int;
}

let id x = x

let parse_monkey text =
  let lines = String.split_on_char '\n' text |> Array.of_list in
  let items_str = lines.(1) |> Str.split (Str.regexp_string ": ") in
  let items =
    List.nth items_str 1
    |> Str.split (Str.regexp_string ", ")
    |> List.map int_of_string
  in
  let operation =
    Scanf.sscanf lines.(2) "  Operation: new = old %s %s" (fun op x ->
        match op with
        | "+" -> (
            match x with
            | "old" -> fun old -> old + old
            | x ->
                let value = int_of_string x in
                fun old -> old + value)
        | "*" -> (
            match x with
            | "old" -> fun old -> old * old
            | x ->
                let value = int_of_string x in
                fun old -> old * value)
        | _ -> failwith "Unrecognized operator")
  in
  let divisor = Scanf.sscanf lines.(3) "  Test: divisible by %d" id in
  let whenTrue = Scanf.sscanf lines.(4) "    If true: throw to monkey %d" id in
  let whenFalse = Scanf.sscanf lines.(5) "    If false: throw to monkey %d" id in
  {
    operation;
    testDivisor = divisor;
    whenTrue;
    whenFalse;
    items;
    inspectCount = 0;
  }

let from_file filename =
  let fh = open_in filename in
  let text = Stdlib.really_input_string fh (in_channel_length fh) in
  let monkeys =
    text
    |> Str.split (Str.regexp_string "\n\n")
    |> List.map parse_monkey |> Array.of_list
  in
  close_in fh;
  monkeys
