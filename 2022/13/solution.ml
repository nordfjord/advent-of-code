let pairs =
  Seq.of_dispenser (fun () ->
      try
        let left = Yojson.Safe.from_string (read_line ()) in
        let right = Yojson.Safe.from_string (read_line ()) in
        (* should be an empty string *)
        match read_line () with
        | _ -> Some (left, right)
        | exception End_of_file -> Some (left, right)
      with End_of_file -> None)
  |> List.of_seq

let rec part1_compare (a : Yojson.Safe.t) (b : Yojson.Safe.t) =
  match (a, b) with
  | `Int a, `Int b -> compare a b
  | `Int _, `List _ -> part1_compare (`List [ a ]) b
  | `List _, `Int _ -> part1_compare a (`List [ b ])
  | `List (a :: as'), `List (b :: bs) -> (
      match part1_compare a b with
      | -1 -> -1
      | 0 -> part1_compare (`List as') (`List bs)
      | 1 -> 1
      | _ -> failwith "WTF2")
  | `List [], `List _ -> -1
  | `List _, `List [] -> 1
  | _ -> failwith "WTF"

let () =
  pairs
  |> List.mapi (fun i (left, right) ->
         let res = part1_compare left right in
         if res = -1 then i + 1 else 0)
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n";

  let divider_packet_1 = `List [ `List [ `Int 2 ] ] in
  let divider_packet_2 = `List [ `List [ `Int 6 ] ] in

  let divider_packets : Yojson.Safe.t list =
    [ divider_packet_1; divider_packet_2 ]
  in

  let sorted =
    pairs
    |> List.concat_map (fun (left, right) -> [ left; right ])
    |> List.append divider_packets
    |> List.sort part1_compare
  in

  sorted |> List.iter (fun x -> print_endline (Yojson.Safe.pretty_to_string x));
  sorted |> List.to_seq
  |> Seq.fold_lefti
       (fun state i value ->
         if value = divider_packet_1 || value = divider_packet_2 then
           state * (i + 1)
         else state)
       1
  |> Printf.printf "Part 2: %d\n"
