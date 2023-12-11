open Printf
open Computer

let input = read_line ()

let part1 () =
  let result = ref 0 in
  for a = 0 to 4 do
    for b = 0 to 4 do
      for c = 0 to 4 do
        for d = 0 to 4 do
          for e = 0 to 4 do
            if [ a; b; c; d; e ] |> List.sort_uniq compare |> List.length = 5
            then (
              let output =
                [ a; b; c; d; e ]
                |> List.map (fun init ->
                  match get_program input with
                  | InputRequested f -> f init
                  | _ -> failwith "Unexpected")
                |> List.fold_left
                     (fun out program ->
                       match program with
                       | InputRequested f ->
                         (match f out with
                          | Out (value, _) -> value
                          | Halted -> out
                          | _ -> failwith "unexpected program behaviour")
                       | Out (value, _) -> value
                       | Halted -> out)
                     0
              in
              result := max !result output)
          done
        done
      done
    done
  done;
  !result |> printf "Part 1: %d\n"

let () = part1 ()

let rec run_until_output cell p =
  match p with
  | InputRequested f -> run_until_output cell (f !cell)
  | Out (value, p) ->
    cell := value;
    p ()
  | Halted -> Halted

let part2 () =
  let result = ref 0 in
  for a = 5 to 9 do
    for b = 5 to 9 do
      for c = 5 to 9 do
        for d = 5 to 9 do
          for e = 5 to 9 do
            if [ a; b; c; d; e ] |> List.sort_uniq compare |> List.length = 5
            then (
              let output = ref 0 in
              let programs =
                ref
                  ([ a; b; c; d; e ]
                   |> List.map (fun init ->
                     match get_program input with
                     | InputRequested f -> f init
                     | _ -> failwith "Unexpected"))
              in
              while !programs |> List.for_all (fun s -> s <> Halted) do
                programs := !programs |> List.map (run_until_output output)
              done;
              result := max !result !output)
          done
        done
      done
    done
  done;
  !result |> printf "Part 2: %d\n"

let () = part2 ()
