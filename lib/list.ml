include Stdlib.List

let rec pairwise = function
  | [] -> []
  | a :: b :: xs -> (a, b) :: pairwise (b :: xs)
  | [ _ ] -> []

let index_of item l =
  let rec inner l i =
    match l with
    | [] -> -1
    | x :: _ when x = item -> i
    | _ :: xs -> inner xs (i + 1)
  in
  inner l 0
