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

type op =
  | Remove of string
  | Add of string * int

let boxes = Array.init 256 ~f:(fun _ -> [])

let () =
  let boxes =
    lines
    |> List.map ~f:(fun str ->
      if String.mem str '-'
      then Remove (String.chop_suffix_exn str ~suffix:"-")
      else (
        match String.split str ~on:'=' with
        | [ s; i ] -> Add (s, Int.of_string i)
        | _ -> failwith "bad input"))
    |> List.fold ~init:boxes ~f:(fun boxes op ->
      (match op with
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
  in
  let sum =
    Array.foldi boxes ~init:0 ~f:(fun i acc box ->
      let box_number = i + 1 in
      let sum =
        List.rev box
        |> List.foldi ~init:0 ~f:(fun i acc (_, focal_length) ->
          let result = box_number * (i + 1) * focal_length in
          result + acc)
      in
      acc + sum)
  in
  printf "%d\n" sum
