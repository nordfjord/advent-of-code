include Stdlib.List

let rec pairwise = function
  | a :: b :: xs -> (a, b) :: pairwise (b :: xs)
  | _ -> []

let rec last = function
  | [] -> raise Not_found
  | [x] -> x
  | _ :: xs -> last xs

let rec take n = function
  | [] -> []
  | x :: xs when n > 0 -> x :: take (n - 1) xs
  | _ -> []

let rec drop n = function
  | [] -> []
  | _ :: xs when n > 0 -> drop (n - 1) xs
  | xs -> xs

let rec at n = function
  | [] -> raise Not_found
  | x :: _ when n = 0 -> x
  | _ :: xs -> at (n - 1) xs

let sum l = fold_left (+) 0 l
let sum_float l = fold_left (+.) 0. l


let index_of item l =
  let rec inner l i =
    match l with
    | [] -> -1
    | x :: _ when x = item -> i
    | _ :: xs -> inner xs (i + 1)
  in
  inner l 0
