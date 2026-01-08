open Base
open Stdio

let sw = Prelude.Stopwatch.start ()
let lines = In_channel.input_lines stdin

module Tech = struct
  type t =
    | Cut of int
    | NewStack
    | Increment of int
  [@@deriving sexp, hash, compare, equal]

  module Parser = struct
    open Angstrom

    let integer = take_while1 (fun c -> Char.(is_digit c || c = '-')) >>| Int.of_string
    let cut = string "cut " *> integer >>| fun n -> Cut n
    let deal_into_new_stack = string "deal into new stack" *> return NewStack

    let deal_with_increment =
      string "deal with increment " *> integer >>| fun n -> Increment n

    let tech = choice [ cut; deal_into_new_stack; deal_with_increment ]

    let parse input =
      List.map input ~f:(fun s ->
        match parse_string ~consume:All tech s with
        | Ok res -> res
        | Error msg -> failwith msg)
  end
end

module Deck = struct
  let create size = Array.init size ~f:Fn.id

  let cut deck n =
    let size = Array.length deck in
    let n = if n < 0 then size + n else n in
    let first_part = Array.sub deck ~pos:n ~len:(size - n) in
    let second_part = Array.sub deck ~pos:0 ~len:n in
    Array.append first_part second_part

  let deal_into_new_stack deck =
    Array.init (Array.length deck) ~f:(fun i -> deck.(Array.length deck - 1 - i))

  let deal_with_increment deck n =
    let size = Array.length deck in
    let new_deck = Array.create ~len:size (-1) in
    Array.iteri deck ~f:(fun i card ->
      let pos = i * n % size in
      new_deck.(pos) <- card);
    new_deck

  let apply_technique deck technique =
    match technique with
    | Tech.Cut n -> cut deck n
    | Tech.NewStack -> deal_into_new_stack deck
    | Tech.Increment n -> deal_with_increment deck n
end

let techniques = Tech.Parser.parse lines

let part1 () =
  let shuffled_deck =
    List.fold techniques ~init:(Deck.create 10007) ~f:(fun d tech ->
      Deck.apply_technique d tech)
  in
  Array.findi shuffled_deck ~f:(fun _ card -> card = 2019)
  |> Option.map ~f:fst
  |> Option.value_exn

let rec gcd_ext a = function
  | 0 -> (1, 0, a)
  | b ->
    let s, t, g = gcd_ext b (a % b) in
    (t, s - (a / b * t), g)

let mod_inv a m =
  let mk_pos x = if x < 0 then x + m else x in
  match gcd_ext a m with
  | i, _, 1 -> mk_pos i
  | _ -> failwith "mod_inv"

let mod_pow base exp modulus =
  let rec aux base exp acc =
    if exp = 0
    then acc
    else if exp % 2 = 1
    then aux (base * base % modulus) (exp / 2) (acc * base % modulus)
    else aux (base * base % modulus) (exp / 2) acc
  in
  aux base exp 1

let part2 () =
  let times = 101741582076661 in
  let deckSize = 119315717514047 in
  let position = 2020 in
  let multiplier, offset =
    techniques
    |> List.fold ~init:(1, 0) ~f:(fun (multiplier, offset) tech ->
      match tech with
      | Tech.Cut n ->
        let offset = (offset + (n * multiplier % deckSize)) % deckSize in
        (multiplier, offset)
      | Tech.NewStack ->
        let multiplier = -multiplier % deckSize in
        let offset = (offset + multiplier) % deckSize in
        (multiplier, offset)
      | Tech.Increment n ->
        let inv = mod_inv n deckSize in
        let multiplier = multiplier * inv % deckSize in
        (multiplier, offset))
  in
  let inc = mod_pow multiplier times deckSize in
  let offset =
    offset * (1 - inc) * mod_inv (1 - (multiplier % deckSize)) deckSize % deckSize
  in
  ((position * inc) + offset) % deckSize

let () =
  printf "%s\n" (Sexp.to_string_hum (List.sexp_of_t Tech.sexp_of_t techniques));
  Prelude.Runner.run part1 part2;
  printf "Execution time: %.3f ms\n" (sw ())
