open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

module Parse = struct
  open Angstrom

  let integer = take_while1 Char.is_digit >>| Int.of_string

  let box =
    let* _ = any_char <* char ':' <* end_of_line in
    let* content = sep_by1 end_of_line (many1 (char '#' <|> char '.')) in
    let size =
      List.fold content ~init:0 ~f:(fun acc row ->
        acc + List.count row ~f:(Char.equal '#'))
    in
    return size

  let boxes = sep_by1 (end_of_line <* end_of_line) box

  let rule =
    let* size_x = integer <* char 'x' in
    let* size_y = integer <* char ':' <* char ' ' in
    let* counts = sep_by1 (char ' ') integer in
    return ((size_x, size_y), counts)

  let rules = sep_by1 end_of_line rule

  let parser =
    let* bs = boxes <* end_of_line <* end_of_line in
    let* rs = rules in
    return (bs, rs)

  let parse_input input = parse_string ~consume:All parser input |> Result.ok_or_failwith
end

let bs, rs =
  let input = In_channel.input_all stdin in
  Parse.parse_input input

let bs = Array.of_list bs

let part1 () =
  List.count rs ~f:(fun ((w, h), counts) ->
    let area = w * h in
    let combined_area =
      List.foldi counts ~init:0 ~f:(fun i acc count -> acc + (count * bs.(i)))
    in
    combined_area <= area)

let part2 () = 0

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
