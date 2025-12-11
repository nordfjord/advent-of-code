open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

module BitArray = struct
  type t = int [@@deriving hash, compare, equal, sexp, show]

  let empty = 0
  let set ba index = ba lor (1 lsl index)
  let toggle ba index = ba lxor (1 lsl index)
  let mem ba index = ba land (1 lsl index) <> 0

  let of_list lst =
    List.foldi lst ~init:empty ~f:(fun i acc bit -> if bit then set acc i else acc)
end

module Machine = struct
  type t =
    { lights : BitArray.t
    ; buttons : int list list
    ; joltage : int list
    }
  [@@deriving show, hash, compare, sexp, equal]
end

let parse_line line =
  let open Angstrom in
  let integer = take_while1 Char.is_digit >>| Int.of_string in
  let light = char '#' >>| fun _ -> true in
  let dark = char '.' >>| fun _ -> false in
  let lights = char '[' *> many1 (light <|> dark) <* char ']' >>| BitArray.of_list in
  let button = char '(' *> sep_by (char ',') integer <* char ')' in
  let joltage = char '{' *> sep_by (char ',') integer <* char '}' in
  let parser =
    let* lights = lights <* char ' ' in
    let* buttons = sep_by (char ' ') button <* char ' ' in
    let* joltage = joltage in
    return Machine.{ lights; buttons; joltage }
  in
  match parse_string ~consume:Prefix parser line with
  | Ok result -> result
  | Error msg -> failwith ("Parse error: " ^ msg)

let lines = In_channel.input_lines stdin |> List.map ~f:parse_line

let part1 () =
  let bfs (m : Machine.t) =
    let q = Queue.create () in
    Queue.enqueue q (BitArray.empty, BitArray.empty, 0);
    let visited = Hash_set.create (module Int) in
    let buttons = Array.of_list m.buttons in
    let rec aux () =
      if Queue.is_empty q
      then failwith "No solution found"
      else (
        let state, pressed, len = Queue.dequeue_exn q in
        if BitArray.equal state m.lights
        then len
        else (
          Array.iteri buttons ~f:(fun i button ->
            let next_state = List.fold button ~init:state ~f:BitArray.toggle in
            let next_pressed = BitArray.set pressed i in
            if (not (BitArray.mem pressed i)) && not (Hash_set.mem visited next_state)
            then (
              Hash_set.add visited next_state;
              Queue.enqueue q (next_state, next_pressed, len + 1)));
          aux ()))
    in
    aux ()
  in
  List.sum (module Int) lines ~f:bfs

(* let p2_dumb () = *)
(*   let bfs (m : Machine.t) = *)
(*     let q = Queue.create () in *)
(*     let end_joltage = List.to_array m.joltage in *)
(*     let joltage_exceeded state = *)
(*       Array.existsi state ~f:(fun i v -> v > end_joltage.(i)) *)
(*     in *)
(*     Queue.enqueue q (Array.create ~len:(Array.length end_joltage) 0, 0); *)
(*     let visited = Hash_set.Poly.create () in *)
(*     let rec aux () = *)
(*       if Queue.is_empty q *)
(*       then failwith "No solution found" *)
(*       else ( *)
(*         let state, len = Queue.dequeue_exn q in *)
(*         if Array.equal Int.equal state end_joltage *)
(*         then len *)
(*         else ( *)
(*           List.iter m.buttons ~f:(fun button -> *)
(*             let next_state = Array.copy state in *)
(*             List.iter button ~f:(fun index -> *)
(*               next_state.(index) <- next_state.(index) + 1); *)
(*             if *)
(*               (not (Hash_set.mem visited next_state)) && not (joltage_exceeded next_state) *)
(*             then ( *)
(*               Hash_set.add visited next_state; *)
(*               Queue.enqueue q (next_state, len + 1))); *)
(*           aux ())) *)
(*     in *)
(*     aux () *)
(*   in *)
(*   List.sum (module Int) lines ~f:bfs *)

let part2 () =
  let solve (m : Machine.t) =
    let open Z3 in
    let ctx = mk_context [] in
    let ( = ) a b = Boolean.mk_eq ctx a b in
    let ( >= ) a b = Arithmetic.mk_ge ctx a b in
    let of_int x = Arithmetic.Integer.mk_numeral_i ctx x in
    let mk_int name = Arithmetic.Integer.mk_const_s ctx name in
    let sum xs = Arithmetic.mk_add ctx xs in
    let opt = Optimize.mk_opt ctx in
    let presses = mk_int "presses" in
    let end_joltage = List.to_array m.joltage |> Array.map ~f:of_int in
    let button_vars =
      Array.init (List.length m.buttons) ~f:(fun i -> mk_int ("b" ^ Int.to_string i))
    in
    let connected_buttons i =
      List.filter_mapi m.buttons ~f:(fun j button ->
        if List.mem button i ~equal:Int.equal then Some button_vars.(j) else None)
    in
    let equations =
      List.mapi m.joltage ~f:(fun i _ ->
        let buttons = connected_buttons i in
        end_joltage.(i) = sum buttons)
    in
    Optimize.add opt equations;
    Optimize.add opt (Array.to_list button_vars |> List.map ~f:(fun bv -> bv >= of_int 0));
    Optimize.add opt [ presses = sum (Array.to_list button_vars) ];
    let _ = Optimize.minimize opt presses in
    let _ = Optimize.check opt in
    let model = Optimize.get_model opt |> Option.value_exn in
    let p = Model.eval model presses true in
    p |> Option.value_exn |> Arithmetic.Integer.get_big_int |> Z.to_int
  in
  List.sum (module Int) lines ~f:solve

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
