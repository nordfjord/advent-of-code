open Printf
open Prelude

let input = read_line ()

let chunk size arr =
  Seq.ints 0
  |> Seq.take (Array.length arr / size)
  |> Seq.map (fun i -> Array.sub arr (i * size) size)
  |> Array.of_seq

let layers =
  let width = 25 in
  Seq.ints 0
  |> Seq.take_while (fun i -> (i * width) + width <= String.length input)
  |> Seq.map (fun i ->
         let value = String.sub input (i * width) width in
         value |> String.to_seq
         |> Seq.map (fun c -> Char.code c - Char.code '0')
         |> Array.of_seq )
  |> Array.of_seq |> chunk 6

let count_digits layers =
  layers |> Array.to_seq
  |> Seq.map (fun layer ->
         Array.to_seq layer
         |> Seq.concat_map Array.to_seq
         |> Seq.fold_left
              (fun (x, y, z) value ->
                match value with
                | 0 -> (x + 1, y, z)
                | 1 -> (x, y + 1, z)
                | 2 -> (x, y, z + 1)
                | _ -> (x, y, z) )
              (0, 0, 0) )

let score (_, y, z) = y * z

let part1 () =
  count_digits layers
  |> Seq.fold_left
       (fun ((x_state, _, _) as state) ((x, _, _) as value) ->
         if x_state > x then value else state )
       (Int.max_int, 0, 0)
  |> score |> printf "Part 1: %d\n"

let () = part1 ()

let print_image arr =
  print_endline "Part 2:"
  ; for i = 0 to Array.length arr - 1 do
      for j = 0 to Array.length arr.(i) - 1 do
        if arr.(i).(j) = 1 then print_char '#' else print_char ' '
      done
      ; print_char '\n'
    done

let part2 () =
  layers |> Array.to_list |> List.rev
  |> List.fold_left
       (fun back front ->
         back
         |> Array.mapi (fun i xs ->
                xs
                |> Array.mapi (fun j x ->
                       match (x, front.(i).(j)) with
                       | x, 2 -> x
                       | _, x -> x ) ) )
       (Array.init 6 (fun _ -> Array.make 25 2))
  |> print_image

let () = part2 ()
