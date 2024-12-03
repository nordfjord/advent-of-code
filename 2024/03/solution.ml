open Base
open Stdio

module Op = struct
  type t =
    | Mul of int * int
    | EnableMul
    | DisableMul
end

module State = struct
  type t =
    { mul_enabled : bool
    ; p1 : int
    ; p2 : int
    }

  let initial = { mul_enabled = true; p1 = 0; p2 = 0 }

  let evolve state e =
    match e with
    | Op.Mul (a, b) when state.mul_enabled ->
      let x = a * b in
      { state with p1 = state.p1 + x; p2 = state.p2 + x }
    | Op.Mul (a, b) -> { state with p1 = state.p1 + (a * b) }
    | Op.EnableMul -> { state with mul_enabled = true }
    | Op.DisableMul -> { state with mul_enabled = false }
end

module Parse = struct
  open Angstrom

  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| Int.of_string

  let mul_expr =
    let* _ = string "mul(" in
    let* a = integer in
    let* _ = char ',' in
    let* b = integer in
    let* _ = char ')' in
    return (Op.Mul (a, b))

  let disable_mul = string "don't()" *> return Op.DisableMul
  let enable_mul = string "do()" *> return Op.EnableMul

  let expr =
    fix (fun expr -> choice [ mul_expr; disable_mul; enable_mul; any_char *> expr ])

  let parse = many expr
end

let lines = In_channel.input_all stdin

let () =
  let exprs =
    Result.ok_or_failwith @@ Angstrom.parse_string ~consume:Prefix Parse.parse lines
  in
  let state = List.fold_left exprs ~init:State.initial ~f:State.evolve in
  printf "Part 1: %d\n" state.p1;
  printf "Part 2: %d\n" state.p2
