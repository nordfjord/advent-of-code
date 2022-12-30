type t = int * int

let compare = compare

let (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let show (x,y) = Printf.sprintf "(%d, %d)" x y

