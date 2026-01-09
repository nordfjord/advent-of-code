open Base
open Stdio

let sw = Prelude.Stopwatch.start ()

module Parse = struct
  open Angstrom

  let integer = take_while1 Char.is_digit >>| Int.of_string
  let name = take_while1 Char.is_alpha
  let input = integer >>= fun n -> char ' ' *> name >>= fun nm -> return (nm, n)
  let inputs = sep_by (string ", ") input

  let output =
    let* n = integer <* char ' ' in
    let* nm = name in
    return (nm, n)

  let parser =
    let* inputs = inputs <* string " => " in
    let* output = output in
    return (inputs, output)

  let parse_line line =
    match parse_string ~consume:All parser line with
    | Ok res -> res
    | Error msg -> failwith ("Parse error: " ^ msg)
end

module Qty = struct
  type t = string * int [@@deriving sexp, hash, compare]
end

let reactions = Hashtbl.create (module String)

let () =
  In_channel.input_lines stdin
  |> List.iter ~f:(fun line ->
    let inputs, (nm, n) = Parse.parse_line line in
    Hashtbl.set reactions ~key:nm ~data:(n, inputs))

let opt_add x = function
  | None -> x
  | Some y -> x + y

let ore_needed (node_type, amount) =
  let state = Hashtbl.create (module String) in
  let rec aux (nt, amt) =
    if String.(nt = "ORE")
    then Hashtbl.update state "ORE" ~f:(opt_add amt)
    else (
      let owned = Hashtbl.find state nt |> Option.value ~default:0 in
      let qty, inputs = Hashtbl.find_exn reactions nt in
      let needed = amt - owned in
      let times = Float.(iround_up_exn (of_int needed / of_int qty)) in
      (* we remove the used amount first, then add the produced amount *)
      Hashtbl.update state nt ~f:(opt_add (-amt));
      Hashtbl.update state nt ~f:(opt_add (times * qty));
      if needed < 0
      then ()
      else
        List.iter inputs ~f:(fun (name, qty) ->
          let total_in_qty = qty * times in
          aux (name, total_in_qty)))
  in
  aux (node_type, amount);
  Hashtbl.find_exn state "ORE"

let part1 () = ore_needed ("FUEL", 1)

let fuel_produced ore_count =
  let rec search low high =
    if low >= high
    then low
    else (
      let mid = (low + high + 1) / 2 in
      let ore_for_mid = ore_needed ("FUEL", mid) in
      if ore_for_mid > ore_count then search low (mid - 1) else search mid high)
  in
  search 1 ore_count

let part2 () = fuel_produced 1_000_000_000_000

let () =
  Prelude.Runner.run part1 part2;
  printf "Execution time: %.3f ms\n" (sw ())
