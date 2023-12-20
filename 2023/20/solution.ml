open Base
open Stdio
open Poly

let lines = In_channel.input_all stdin

module Circuit = struct
  type pulse =
    | Low
    | High
  [@@deriving sexp]

  let show_pulse = function
    | Low -> "low"
    | High -> "high"

  let invert_pulse = function
    | Low -> High
    | High -> Low

  type circuit_type =
    | FlipFlop of pulse
    | Conjuction of (string, pulse) Hashtbl.t
    | Broadcaster

  type t =
    { type_ : circuit_type
    ; id : string
    ; dest : string list
    }
end

module Parse = struct
  open Angstrom

  let arrow = string " -> "
  let alpha = take_while1 Char.is_alpha

  let circuit_type =
    choice
      [ char '%' *> return (Circuit.FlipFlop Circuit.Low)
      ; (char '&' >>| fun _ -> Circuit.Conjuction (Hashtbl.Poly.create ()))
      ; string "broadcaster" *> return Circuit.Broadcaster
      ]

  let broadcaster =
    let* dest = string "broadcaster" *> arrow *> sep_by (string ", ") alpha in
    return { Circuit.type_ = Broadcaster; id = "broadcaster"; dest }

  let circuit =
    let* type_ = circuit_type in
    let* id = alpha in
    let* dest = arrow *> sep_by (string ", ") alpha in
    return { Circuit.type_; id; dest }

  let input = sep_by end_of_line (broadcaster <|> circuit)
  let parse s = parse_string ~consume:All input s |> Result.ok_or_failwith
end

let circuits =
  Parse.parse lines
  |> List.map ~f:(fun c -> (c.Circuit.id, c))
  |> Hashtbl.Poly.of_alist_exn

let () =
  circuits
  |> Hashtbl.iteri ~f:(fun ~key:id ~data:c ->
    List.iter c.dest ~f:(fun dest ->
      match Hashtbl.find circuits dest with
      | Some { type_ = Conjuction tbl; _ } -> Hashtbl.set tbl ~key:id ~data:Circuit.Low
      | _ -> ()))

let simulate ?(debug = false) i pulses () =
  let q = Queue.create () in
  Queue.enqueue q ("button", "broadcaster", Circuit.Low);
  let count_low = ref 0 in
  let count_high = ref 0 in
  while not (Queue.is_empty q) do
    let src, dst, pulse = Queue.dequeue_exn q in
    if pulse = Circuit.Low
    then count_low := !count_low + 1
    else count_high := !count_high + 1;
    if dst = "jz" && pulse = Circuit.High then Hashtbl.set pulses ~key:src ~data:i;
    if Hashtbl.mem circuits dst
    then (
      let circuit = Hashtbl.find_exn circuits dst in
      match circuit.type_ with
      | Circuit.FlipFlop t when pulse = Circuit.Low ->
        let new_pulse = Circuit.invert_pulse t in
        Hashtbl.set circuits ~key:dst ~data:{ circuit with type_ = FlipFlop new_pulse };
        List.iter circuit.dest ~f:(fun dest ->
          if debug
          then printf "%s -%s> %s\n" circuit.id (Circuit.show_pulse new_pulse) dest;
          Queue.enqueue q (circuit.id, dest, new_pulse))
      | Circuit.FlipFlop _ -> ()
      | Circuit.Conjuction tbl ->
        Hashtbl.set tbl ~key:src ~data:pulse;
        if debug
        then
          Hashtbl.iteri tbl ~f:(fun ~key:k ~data:v ->
            printf "mem[%s] = %s\n" k (Circuit.show_pulse v));
        let new_pulse =
          if Hashtbl.for_all tbl ~f:(fun x -> x = Circuit.High)
          then Circuit.Low
          else Circuit.High
        in
        List.iter circuit.dest ~f:(fun dest ->
          if debug
          then printf "%s -%s> %s\n" circuit.id (Circuit.show_pulse new_pulse) dest;
          Queue.enqueue q (circuit.id, dest, new_pulse))
      | Circuit.Broadcaster ->
        List.iter circuit.dest ~f:(fun dest ->
          if debug then printf "%s -%s> %s\n" circuit.id (Circuit.show_pulse pulse) dest;
          Queue.enqueue q (circuit.id, dest, pulse)))
  done;
  (!count_low, !count_high)

let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a * b / gcd a b

let () =
  let count_low = ref 0 in
  let count_high = ref 0 in
  let pulses = Hashtbl.create (module String) in
  let i = ref 1 in
  while Hashtbl.length pulses < 4 do
    let low, high = simulate !i pulses () in
    if !i <= 1000
    then (
      count_low := !count_low + low;
      count_high := !count_high + high);
    i := !i + 1
  done;
  printf "%d\n%!" (!count_low * !count_high);
  Hashtbl.fold pulses ~init:1 ~f:(fun ~key:_ ~data:v acc -> lcm acc v) |> printf "%d\n"
[@@warning "-8"]
