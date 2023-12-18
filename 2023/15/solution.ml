open Base
open Stdio

let lines = In_channel.input_lines stdin |> List.hd_exn |> String.split ~on:','

let hash str =
  String.fold str ~init:0 ~f:(fun acc c ->
    let code = Char.to_int c in
    let increased = acc + code in
    let mult = increased * 17 in
    let rem = mult % 256 in
    rem)

(* part 1 *)
let () = lines |> List.sum (module Int) ~f:hash |> printf "%d\n"

(* part 2 *)

type op =
  | Remove of string
  | Add of string * int

let boxes = Array.init 256 ~f:(fun _ -> [])

let double_array arr =
  let len = Array.length arr in
  let new_len = len * 2 in
  let new_arr = Array.create ~len:new_len [||] in
  Array.blit ~src:arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len;
  new_arr

let parse str =
  if String.mem str '-'
  then Remove (String.chop_suffix_exn str ~suffix:"-")
  else (
    match String.split str ~on:'=' with
    | [ s; i ] -> Add (s, Int.of_string i)
    | _ -> failwith "bad input")

let () =
  lines
  |> List.fold ~init:boxes ~f:(fun boxes x ->
    (match parse x with
     | Remove s ->
       let h = hash s in
       let box = boxes.(h) in
       boxes.(h) <- List.filter box ~f:(fun (s', _) -> not @@ String.equal s s')
     | Add (s, i) ->
       let h = hash s in
       let box = boxes.(h) in
       boxes.(h)
       <- (if box |> List.exists ~f:(fun (s', _) -> String.equal s s')
           then
             box
             |> List.map ~f:(fun (s', i') ->
               if String.equal s s' then (s, i) else (s', i'))
           else (s, i) :: box));
    boxes)
  |> Array.foldi ~init:0 ~f:(fun i acc box ->
    let box_number = i + 1 in
    let sum =
      List.rev box
      |> List.foldi ~init:0 ~f:(fun i acc (_, focal_length) ->
        let result = box_number * (i + 1) * focal_length in
        result + acc)
    in
    acc + sum)
  |> printf "%d\n";
  Array.max_elt boxes ~compare:(fun a b -> List.length a - List.length b)
  |> Option.map ~f:List.length
  |> Option.iter ~f:(printf "len: %d\n")
