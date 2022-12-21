let simulateRound worry_modifier divisor_product monkeys =
  let open Monkey in
  monkeys
  |> List.iter (fun monkey ->
         monkey.items
         |> List.iter (fun item ->
                monkey.inspect_count <- monkey.inspect_count + 1;
                let item = item mod divisor_product in
                let op_result = monkey.operation item in
                let new_worry = worry_modifier op_result in
                let new_monkey =
                  if new_worry mod monkey.divisor = 0 then
                    List.nth monkeys monkey.on_true
                  else List.nth monkeys monkey.on_false
                in
                new_monkey.items <- new_worry :: new_monkey.items);
         monkey.items <- [])

let compare_desc a b = compare b a
let product f arr = arr |> List.fold_left (fun sum item -> sum * f item) 1

let run rounds worry_modifier =
  let monkeys = Monkey.from_file Sys.argv.(1) in
  let divisor_product = monkeys |> product Monkey.get_divisor in
  for _ = 1 to rounds do
    simulateRound worry_modifier divisor_product monkeys
  done;
  let inspectCounts =
    monkeys |> List.map Monkey.get_inspect_count |> List.sort compare_desc
  in
  let a, b = (List.nth inspectCounts 0, List.nth inspectCounts 1) in
  a * b

let worry_modifier x = x / 3
let id x = x
let part1 () = run 20 worry_modifier
let part2 () = run 10000 id

let () =
  Printf.printf "Part 1: %d\n" (part1 ());
  Printf.printf "Part 2: %d\n" (part2 ())
