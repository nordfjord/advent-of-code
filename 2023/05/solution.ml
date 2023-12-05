let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq

module Range = struct
  type t = int * int [@@deriving show]

  let length (s, e) = e - s
  let translate n (s, e) = (s + n, e + n)
  let before (s1, e1) (s2, _e2) = (s1, min e1 s2)
  let after (s1, e1) (_s2, e2) = (max e2 s1, e1)
  let inter (s1, e1) (s2, e2) = (max s1 s2, min e1 e2)
  let contains n (s, e) = s <= n && n < e
  let start = fst
  let end_ = snd
  let is_valid (s, e) = s < e
end

module Ranges = struct
  type t = Range.t list [@@deriving show]
end

module Mapping = struct
  type t =
    { src : Range.t
    ; dst : Range.t
    }
  [@@deriving show]

  let offset x = Range.start x.dst - Range.start x.src
  let translate_opt n x = if Range.contains n x.src then Some (n + offset x) else None

  let translate_range range x =
    let before = Range.before range x.src in
    let after = Range.after range x.src in
    let inter = Range.inter range x.src in
    (before, Range.translate (offset x) inter, after)
end

module Mappings = struct
  type t = Mapping.t list [@@deriving show]

  let rec convert n = function
    | [] -> n
    | x :: xs ->
      (match Mapping.translate_opt n x with
       | None -> convert n xs
       | Some n -> n)

  let rec apply_mapping mapping (result, next_ranges) ranges =
    match ranges with
    | [] -> (result, next_ranges)
    | range :: ranges ->
      let before, inter, after = Mapping.translate_range range mapping in
      let next_ranges =
        if Range.is_valid before then before :: next_ranges else next_ranges
      in
      let next_ranges =
        if Range.is_valid after then after :: next_ranges else next_ranges
      in
      let result = if Range.is_valid inter then inter :: result else result in
      apply_mapping mapping (result, next_ranges) ranges

  let apply_range ranges mappings =
    let rec aux mappings result ranges =
      match (ranges, mappings) with
      | _, [] | [], _ ->
        let res = result @ ranges in
        res
      | ranges, mapping :: mappings ->
        let result, next_ranges = apply_mapping mapping (result, []) ranges in
        aux mappings result next_ranges
    in
    aux mappings [] ranges
end

module Parse = struct
  let rec parse_section lines i =
    if i >= Array.length lines || lines.(i) = ""
    then []
    else (
      let line = lines.(i) in
      let [ dst; src; count ] =
        line |> String.split_on_char ' ' |> List.map int_of_string
      in
      let src = (src, src + count) in
      let dst = (dst, dst + count) in
      { Mapping.src; dst } :: parse_section lines (i + 1))
    [@@ocaml.warning "-8"]

  let rec parse_sections lines i =
    if i >= Array.length lines
    then []
    else if lines.(i) = ""
    then parse_sections lines (i + 1)
    else (
      let ranges = parse_section lines (i + 1) in
      ranges :: parse_sections lines (i + 1 + List.length ranges))
    [@@ocaml.warning "-8"]

  let parse lines =
    let [ _; seeds ] = lines.(0) |> Str.split (Str.regexp_string ": ") in
    let seeds = seeds |> String.split_on_char ' ' |> List.map int_of_string in
    let sections = parse_sections lines 2 in
    (seeds, sections)
    [@@ocaml.warning "-8"]
end

let seeds, mappings = Parse.parse lines
let find_location seed = mappings |> List.fold_left Mappings.convert seed

let part1 () =
  seeds
  |> List.map find_location
  |> List.fold_left min max_int
  |> Printf.printf "Part 1: %d\n%!"

let rec pairs = function
  | [] -> []
  | [ _ ] -> []
  | x :: y :: xs -> (x, y) :: pairs xs

let part2_stupid () =
  seeds
  |> pairs
  |> List.to_seq
  |> Seq.concat_map (fun (start, count) ->
       Seq.ints start |> Seq.take count |> Seq.map find_location)
  |> Seq.fold_left min max_int
  |> Printf.printf "Part 2: %d\n"

let tap f x =
  f x;
  x

let part2_smart () =
  let seed_ranges =
    seeds |> pairs |> List.map (fun (start, count) -> (start, start + count))
  in
  let result =
    seed_ranges
    |> List.map (fun range -> mappings |> List.fold_left Mappings.apply_range [ range ])
  in
  result
  |> List.concat
  |> List.to_seq
  |> Seq.map Range.start
  |> Seq.fold_left min max_int
  |> Printf.printf "Part 2: %d\n"

let () =
  part1 ();
  part2_smart ()
