open Base
open Stdio

let lines = In_channel.input_all stdin

module Part = struct
  type t =
    { x : int
    ; m : int
    ; a : int
    ; s : int
    }
  [@@deriving sexp, compare]

  let get a x =
    match a with
    | `x -> x.x
    | `m -> x.m
    | `a -> x.a
    | `s -> x.s
end

type op =
  | Lt
  | Gt

type rule =
  | Direct of string
  | Rule of
      { attr : [ `x | `m | `a | `s ]
      ; op : op
      ; value : int
      ; next : string
      }

module Parse = struct
  open Angstrom

  let int = take_while1 Char.is_digit >>| Int.of_string
  let op = choice [ char '>' *> return Gt; char '<' *> return Lt ]

  let attr =
    choice
      [ char 'x' *> return `x
      ; char 'm' *> return `m
      ; char 'a' *> return `a
      ; char 's' *> return `s
      ]

  let direct_rule =
    let* next = take_while Char.is_alpha in
    return (Direct next)

  let op_rule =
    let* attr = attr in
    let* op = op in
    let* value = int in
    let* next = char ':' *> take_while Char.is_alpha in
    return (Rule { attr; op; value; next })

  let rule = choice [ op_rule; direct_rule ]

  let workflow =
    let* id = take_while1 (Fn.non (Char.equal '{')) <* char '{' in
    let* rules = sep_by1 (char ',') rule <* char '}' in
    return (id, rules)

  let part =
    let* x = string "{x=" *> int <* char ',' in
    let* m = string "m=" *> int <* char ',' in
    let* a = string "a=" *> int <* char ',' in
    let* s = string "s=" *> int <* char '}' in
    return { Part.x; m; a; s }

  let parse =
    let* workflows = sep_by1 end_of_line workflow <* end_of_line in
    let* parts = end_of_line *> sep_by1 end_of_line part <* end_of_input in
    return (Map.Poly.of_alist_exn workflows, parts)
end

let workflows, parts =
  Angstrom.parse_string ~consume:All Parse.parse lines |> Result.ok_or_failwith

type rule_result =
  | A
  | R
[@@deriving sexp, compare, equal]

let rec run_rules rules part =
  match rules with
  | [] -> failwith "no rules"
  | Direct next :: _ -> next
  | Rule rule :: rules ->
    let attr = Part.get rule.attr part in
    (match rule.op with
     | Lt when attr < rule.value -> rule.next
     | Gt when attr > rule.value -> rule.next
     | _ -> run_rules rules part)

let rec run_workflow workflows id part =
  let rules = Map.find_exn workflows id in
  match run_rules rules part with
  | "A" -> A
  | "R" -> R
  | next -> run_workflow workflows next part

let () =
  parts
  |> List.filter ~f:(fun part -> equal_rule_result (run_workflow workflows "in" part) A)
  |> List.sum (module Int) ~f:(fun part -> part.x + part.m + part.a + part.s)
  |> printf "%d\n"

module PartRange = struct
  type t =
    { x : int * int
    ; m : int * int
    ; a : int * int
    ; s : int * int
    }
  [@@deriving sexp, compare, equal]

  let set attr value range =
    match attr with
    | `x -> { range with x = value }
    | `m -> { range with m = value }
    | `a -> { range with a = value }
    | `s -> { range with s = value }

  let get attr range =
    match attr with
    | `x -> range.x
    | `m -> range.m
    | `a -> range.a
    | `s -> range.s

  let size range =
    let x = fst range.x - snd range.x in
    let m = fst range.m - snd range.m in
    let a = fst range.a - snd range.a in
    let s = fst range.s - snd range.s in
    x * m * a * s

  let make initial = { x = initial; m = initial; a = initial; s = initial }
end

let rec run_rules rules range =
  match rules with
  | [] -> failwith "no rules"
  | Direct x :: _ -> find_acceptable_ranges workflows x range
  | Rule rule :: rules ->
    let min, max = PartRange.get rule.attr range in
    (match rule.op with
     | Lt when max <= rule.value ->
       (* Full match *)
       find_acceptable_ranges workflows rule.next range
     | Lt when min < rule.value ->
       (* Partial match
          if value = 2000; max = 4000; min = 1000
          then we want to match 1000-2000 (ranges are exclusive)
          and run the next rule on 2001-4000
       *)
       let matched =
         PartRange.set rule.attr (min, rule.value) range
         |> find_acceptable_ranges workflows rule.next
       in
       let unmatched =
         PartRange.set rule.attr (rule.value, max) range |> run_rules rules
       in
       matched + unmatched
     | Gt when min > rule.value ->
       (* Full match *)
       find_acceptable_ranges workflows rule.next range
     | Gt when max > rule.value + 1 ->
       (* Partial match
          if value = 2000; max = 4000; min = 1000
          then we want to match 2001-4000
          and run the next rule on 1000-2000
       *)
       let matched =
         PartRange.set rule.attr (rule.value + 1, max) range
         |> find_acceptable_ranges workflows rule.next
       in
       let unmatched =
         PartRange.set rule.attr (min, rule.value + 1) range |> run_rules rules
       in
       matched + unmatched
     | _ -> run_rules rules range)

and find_acceptable_ranges workflows id range =
  match id with
  | "A" -> PartRange.size range
  | "R" -> 0
  | id ->
    let rules = Map.find_exn workflows id in
    run_rules rules range

let () =
  (* range end is exclusive *)
  let range = PartRange.make (1, 4001) in
  find_acceptable_ranges workflows "in" range |> printf "%d\n"
