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
  (* pairs
     |> Seq.iter (fun (a, b) -> print_endline (
       Yojson.Safe.pretty_to_string a
       ^ ", " ^ Yojson.Safe.pretty_to_string b ^ "\n"
     )); *)
  pairs
  |> Seq.iteri (fun i (left, right) ->
         let res = part1_compare left right in
         Printf.printf "%s\n%s\nPair %d: %s\n"
           (Yojson.Safe.pretty_to_string left)
           (Yojson.Safe.pretty_to_string right)
           (i + 1) (if res = -1 then "In the right order" else "Not in the right order"))
