open Monkey

let simulateRound worry_modifier divisor_product monkeys =
  monkeys
  |> Array.iter (fun monkey ->
         monkey.items
         |> List.iter (fun item ->
                monkey.inspectCount <- monkey.inspectCount + 1;
                let item = item mod divisor_product in
                let op_result = monkey.operation item in
                let new_worry = worry_modifier op_result in
                let new_monkey =
                  if new_worry mod monkey.testDivisor = 0 then
                    monkeys.(monkey.whenTrue)
                  else monkeys.(monkey.whenFalse)
                in
                new_monkey.items <- new_worry :: new_monkey.items);
         monkey.items <- [])

let compare_desc a b = compare b a

let print_monkeys monkeys =
  monkeys
  |> Array.iteri (fun i monkey ->
         print_endline
           ("Monkey " ^ string_of_int i ^ ": "
           ^ (monkey.items |> List.map string_of_int |> String.concat ", ")
           ^ " ("
           ^ string_of_int monkey.inspectCount
           ^ ")"))

let run rounds worry_modifier =
  let monkeys = monkeys () in
  let divisor_product =
    monkeys |> Array.fold_left (fun sum m -> sum * m.testDivisor) 1
  in
  for round = 1 to rounds do
    simulateRound worry_modifier divisor_product monkeys
  done;
  let inspectCounts = monkeys |> Array.map (fun x -> x.inspectCount) in
  Array.sort compare_desc inspectCounts;
  let a, b = (inspectCounts.(0), inspectCounts.(1)) in
  a * b

let part1_modifier x = x / 3
let id x = x
let part1 () = run 20 part1_modifier
let part2 () = run 10000 id
let () = print_endline ("\n\nPart 1: " ^ (part1 () |> string_of_int))
let () = print_endline ("Part 2: " ^ (part2 () |> string_of_int))
