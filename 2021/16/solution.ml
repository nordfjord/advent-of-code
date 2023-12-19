open Base
open Stdio

let input = In_channel.input_all stdin

let hex_to_string = function
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' -> "1010"
  | 'B' -> "1011"
  | 'C' -> "1100"
  | 'D' -> "1101"
  | 'E' -> "1110"
  | 'F' -> "1111"
  | _ -> failwith "illegal char"

let input = input |> String.to_list |> List.map ~f:hex_to_string |> String.concat

type op =
  | Sum
  | Product
  | Min
  | Max
  | GreaterThan
  | LessThan
  | EqualTo
[@@deriving sexp, show]

let op_of_int = function
  | 0 -> Sum
  | 1 -> Product
  | 2 -> Min
  | 3 -> Max
  | 5 -> GreaterThan
  | 6 -> LessThan
  | 7 -> EqualTo
  | _ -> failwith "illegal op"

type packet =
  | Literal of int * int
  | Operator of int * op * packet list
[@@deriving sexp, show]

module Parse = struct
  open Angstrom

  let bin_to_dec = List.fold ~init:0 ~f:(fun acc x -> (acc * 2) + x)
  let digit = choice [ char '0' *> return 0; char '1' *> return 1 ]

  let literal_num =
    fix (fun literal_num ->
      let* header = digit in
      match header with
      | 1 ->
        let* g = count 4 digit in
        let* rest = literal_num in
        return (g @ rest)
      | 0 ->
        let* g = count 4 digit in
        return g
      | _ -> fail "illegal literal header")

  let parse_packet =
    fix (fun parse_packet ->
      let literal =
        let* version = count 3 digit >>| bin_to_dec in
        let* _ = string "100" in
        let* n = literal_num >>| bin_to_dec in
        return (Literal (version, n))
      in
      let operator =
        let* version = count 3 digit >>| bin_to_dec in
        let* type_ = count 3 digit >>| bin_to_dec >>| op_of_int in
        let* length_type_id = digit in
        match length_type_id with
        | 0 ->
          let* total_length = count 15 digit >>| bin_to_dec in
          let* to_parse = take total_length in
          return
            (match Angstrom.parse_string ~consume:All (many parse_packet) to_parse with
             | Ok packets -> Operator (version, type_, packets)
             | Error msg -> failwith msg)
        | 1 ->
          let* total_length = count 11 digit >>| bin_to_dec in
          let* packets = count total_length parse_packet in
          return (Operator (version, type_, packets))
        | _ -> fail "illegal operator length type id"
      in
      choice [ literal; operator ])
end

let rec sum_versions packet =
  match packet with
  | Literal (version, _) -> version
  | Operator (version, _, packets) ->
    version + List.sum (module Int) ~f:sum_versions packets

let packet =
  match Angstrom.parse_string ~consume:Prefix Parse.parse_packet input with
  | Ok packets -> packets
  | Error msg -> failwith msg

let rec eval packet =
  match packet with
  | Literal (_, n) -> n
  | Operator (_, op, packets) ->
    let values = List.map ~f:eval packets in
    (match op with
     | Sum -> List.fold values ~init:0 ~f:Int.( + )
     | Product -> List.fold values ~init:1 ~f:( * )
     | Min -> List.fold values ~init:Int.max_value ~f:Int.min
     | Max -> List.fold values ~init:Int.min_value ~f:Int.max
     | GreaterThan ->
       (match values with
        | [ x; y ] -> Bool.to_int (x > y)
        | _ -> failwith "illegal greater than")
     | LessThan ->
       (match values with
        | [ x; y ] -> Bool.to_int (x < y)
        | _ -> failwith "illegal less than")
     | EqualTo ->
       (match values with
        | [ x; y ] -> Bool.to_int (x = y)
        | _ -> failwith "illegal equal to"))

let () =
  printf "%s\n" (Sexp.to_string_hum (sexp_of_packet packet));
  printf "%d\n" (sum_versions packet);
  printf "%d\n" (eval packet)
