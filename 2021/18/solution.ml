open Base
open Stdio

type num =
  | Int of int
  | Pair of num * num
[@@deriving sexp, equal]

module Parse = struct
  open Angstrom

  let int = take_while1 Char.is_digit >>| fun s -> Int (Int.of_string s)

  let pair =
    fix (fun pair ->
      let int_or_pair = int <|> pair in
      let* first = char '[' *> int_or_pair <* char ',' in
      let* second = int_or_pair <* char ']' in
      return (Pair (first, second)))

  let parse s = parse_string ~consume:All pair s |> Result.ok_or_failwith
end

let lines = In_channel.input_lines stdin |> List.map ~f:Parse.parse

let rec add_left a b =
  match b with
  | Int b -> Int (a + b)
  | Pair (l, r) -> Pair (add_left a l, r)

let rec add_right a b =
  match b with
  | Int b -> Int (a + b)
  | Pair (l, r) -> Pair (l, add_right a r)

let explode n =
  let rec aux depth n =
    match n with
    | Int _ -> (false, n, 0, 0)
    | Pair (Int a, Int b) when depth = 4 -> (true, Int 0, a, b)
    | Pair (Int _, Int _) -> (false, n, 0, 0)
    | Pair (l, r) ->
      (match aux (depth + 1) l with
       | true, lhs, lhs_l, lhs_r -> (true, Pair (lhs, add_left lhs_r r), lhs_l, 0)
       | _ ->
         (match aux (depth + 1) r with
          | true, rhs, rhs_l, rhs_r -> (true, Pair (add_right rhs_l l, rhs), 0, rhs_r)
          | _ -> (false, n, 0, 0)))
  in
  let exploded, n, _, _ = aux 0 n in
  (exploded, n)

let rec split n =
  match n with
  | Int x when x < 10 -> (false, n)
  | Int n -> (true, Pair (Int (n / 2), Int ((n + 1) / 2)))
  | Pair (l, r) ->
    (match split l with
     | true, n -> (true, Pair (n, r))
     | false, _ ->
       (match split r with
        | true, n -> (true, Pair (l, n))
        | false, _ -> (false, n)))

(* - If any pair is nested inside four pairs, the leftmost such pair explodes.
   - If any regular number is 10 or greater, the leftmost such regular number splits. *)
let rec reduce n =
  match explode n with
  | true, n -> reduce n
  | false, _ ->
    (match split n with
     | true, n -> reduce n
     | false, _ -> n)

let add a b = Pair (a, b) |> reduce

let rec magnitude n =
  match n with
  | Int n -> n
  | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)

let () =
  lines |> List.reduce ~f:add |> Option.value_exn |> magnitude |> printf "%d\n";
  Sequence.of_list lines
  |> Sequence.cartesian_product (Sequence.of_list lines)
  |> Sequence.filter ~f:(fun (a, b) -> not (equal_num a b))
  |> Sequence.fold ~init:0 ~f:(fun acc (a, b) ->
    let l = magnitude (add a b) in
    let r = magnitude (add b a) in
    max acc @@ max l r)
  |> printf "%d\n"
