open Printf

let numbers =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None)
  |> Seq.map int_of_string |> Array.of_seq

let modulo a b = ((a mod b) + b) mod b

let find_index f arr =
  let rec aux i = if f arr.(i) then i else aux (i + 1) in

  aux 0

let show_array arr =
  printf "[%s]\n"
    (Array.map snd arr |> Array.map string_of_int |> Array.to_list
   |> String.concat ", ")


let mix numbers result =
  let len = Array.length result in

  let simple_swap arr i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  in

  let swap arr i j =
    let len = Array.length arr in
    if i = 0 && j = -1 then
      for i = 0 to len - 2 do
        simple_swap arr i (i + 1)
      done
    else if i = len - 1 && j = len then
      for i = len - 1 downto 1 do
        simple_swap arr i (i - 1)
      done
    else
      let i = modulo i len in
      let j = modulo j len in
      simple_swap arr i j
  in

  for i = 0 to len - 1 do
    let value = numbers.(i) in
    let from_idx = find_index (fun (j, _) -> j = i) result in
    let to_idx = from_idx + (value mod (len - 1)) in

    printf "Moving %d from %d to %d\n" value from_idx to_idx;
    (if value = 0 then ()
    else
      let current_idx = ref from_idx in
      let sign = if value < 0 then -1 else 1 in
      if value < 0 then
        for _ = value to 0 do
          swap result !current_idx (!current_idx + sign);
          current_idx := !current_idx + sign
        done
      else
        for _ = 1 to value do
          swap result !current_idx (!current_idx + sign);
          current_idx := !current_idx + 1
        done);
    show_array result
  done

(* Maybe we can swap repeatedly to move elements in the array *)

let part1 () =
  printf "\n";
  let result = numbers |> Array.mapi (fun i x -> (i, x)) in
  mix numbers result;
  let len = Array.length result in

  let zero_idx = find_index (fun (_, v) -> v = 0) result in
  let x = snd result.((zero_idx + 1000) mod len) in
  let y = snd result.((zero_idx + 2000) mod len) in
  let z = snd result.((zero_idx + 3000) mod len) in
  let sum = x + y + z in
  printf "x=%d; y=%d; z=%d\n" x y z;
  printf "Part 1: %d\n%!" sum

let part2 () =
  let key = 811589153 in
  let numbers = numbers |> Array.map (( * ) key) in
  let result = numbers |> Array.mapi (fun i x -> (i, x)) in
  show_array result;
  for i = 1 to 10 do
    mix numbers result;
    printf "\nAfter %d round of mixing\n" i;
    show_array result;
    printf "\n"
  done;
  let len = Array.length result in

  let zero_idx = find_index (fun (_, v) -> v = 0) result in
  let x = snd result.((zero_idx + 1000) mod len) in
  let y = snd result.((zero_idx + 2000) mod len) in
  let z = snd result.((zero_idx + 3000) mod len) in
  let sum = x + y + z in
  printf "x=%d; y=%d; z=%d\n" x y z;
  printf "Part 2: %d\n" sum

let () =
  part1 ();
  part2 ()
