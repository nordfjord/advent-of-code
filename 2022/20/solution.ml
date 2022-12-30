open Printf

let numbers =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None )
  |> Seq.map int_of_string |> Array.of_seq

let score (arr : int array) =
  let l = Array.length arr in
  let zi =
    arr |> Array.to_seqi
    |> Seq.find_map (fun (i, v) -> if v = 0 then Some i else None)
    |> Option.get
  in
  arr.((zi + 1000) mod l) + arr.((zi + 2000) mod l) + arr.((zi + 3000) mod l)

let remove_at arr i =
  let len = Array.length arr in
  let left = Array.sub arr 0 i in
  let right = Array.sub arr ((i + 1) mod len) (len - i - 1) in
  Array.append left right

let insert_at arr i value =
  let len = Array.length arr in
  let left = Array.sub arr 0 i in
  let right = Array.sub arr i (len - i) in
  Array.concat [left; [|value|]; right]

let switch arr i item =
  let removed = remove_at arr i in
  let l = Array.length removed - 1 in
  let v, _ = item in
  let n = (i + v) mod l in
  let ni = if n <= 0 then l + n else n in
  insert_at removed ni item

let mix original arr =
  original
  |> Array.fold_left
       (fun res num ->
         let i =
           res |> Array.to_seqi
           |> Seq.find_map (fun (i, value) ->
                  if value = num then Some i else None )
           |> Option.get
         in
         let arr = switch res i num in
         arr )
       arr

let shuffle numbers rounds encrypt_key =
  let arr = numbers |> Array.mapi (fun i l -> (l * encrypt_key, i)) in
  let original = Array.copy arr in
  Seq.ints 0 |> Seq.take rounds
  |> Seq.fold_left (fun arr _ -> mix original arr) arr
  |> Array.map fst |> score

(* Maybe we can swap repeatedly to move elements in the array *)

let part1 () =
  printf "\n" ;
  shuffle numbers 1 1 |> printf "Part 1: %d\n"

let part2 () =
  let key = 811589153 in
  shuffle numbers 10 key |> printf "Part 2: %d\n"

let () = part1 () ; part2 ()
