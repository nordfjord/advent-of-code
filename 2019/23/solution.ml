open Base
open Stdio

let sw = Prelude.Stopwatch.start ()
let computer = In_channel.input_line stdin |> Option.value_exn |> Computer.get_computer

module Q = Queue

module NetworkedComputer = struct
  type t =
    { mutable cont : unit -> Computer.run_result
    ; input_queue : int Q.t
    ; output_queue : int Q.t
    }

  type res =
    | Packet of int * int * int
    | Continue
    | AwaitingInput
    | ProcessedInput
    | Halted

  let create addr =
    let comp = Computer.copy computer in
    let q_in = Q.create () in
    let q_out = Q.create () in
    Q.enqueue q_in addr;
    { cont = (fun () -> Computer.run comp); input_queue = q_in; output_queue = q_out }

  let next c =
    match c.cont () with
    | Computer.Out (x, f) ->
      c.cont <- f;
      Q.enqueue c.output_queue x;
      if Q.length c.output_queue = 3
      then (
        let dest = Q.dequeue_exn c.output_queue in
        let x = Q.dequeue_exn c.output_queue in
        let y = Q.dequeue_exn c.output_queue in
        Packet (dest, x, y))
      else Continue
    | Computer.InputRequested f ->
      (match Q.dequeue c.input_queue with
       | None ->
         c.cont <- (fun () -> f (-1));
         AwaitingInput
       | Some v ->
         c.cont <- (fun () -> f v);
         ProcessedInput)
    | Computer.Halted -> Halted
end

let part1 () =
  let computers = Array.init 50 ~f:(fun addr -> NetworkedComputer.create addr) in
  let router (dest, x, y) =
    let comp = computers.(dest) in
    Q.enqueue comp.input_queue x;
    Q.enqueue comp.input_queue y
  in
  let rec loop () =
    let res =
      Array.fold computers ~init:0 ~f:(fun acc c ->
        match NetworkedComputer.next c with
        | Packet (dest, x, y) ->
          if dest = 255
          then y
          else (
            router (dest, x, y);
            acc)
        | _ -> acc)
    in
    if res <> 0 then res else loop ()
  in
  loop ()

let part2 () =
  let computers = Array.init 50 ~f:(fun addr -> NetworkedComputer.create addr) in
  let nat = ref (0, 0) in
  let prev_nat_y = ref 0 in
  let router (dest, x, y) =
    if dest = 255
    then nat := (x, y)
    else (
      let comp = computers.(dest) in
      Q.enqueue comp.input_queue x;
      Q.enqueue comp.input_queue y)
  in
  let rec loop count =
    let next_count =
      Array.count computers ~f:(fun c ->
        match NetworkedComputer.next c with
        | Packet (dest, x, y) ->
          if dest = 255 then nat := (x, y) else router (dest, x, y);
          false
        | AwaitingInput -> true
        | _ -> false)
    in
    if count = next_count && count = 50
    then
      if snd !nat = !prev_nat_y
      then !prev_nat_y
      else (
        prev_nat_y := snd !nat;
        router (0, fst !nat, snd !nat);
        loop 0)
    else loop next_count
  in
  loop 0

let () =
  Prelude.Runner.run part1 part2;
  printf "Execution time: %.3f ms\n" (sw ())
