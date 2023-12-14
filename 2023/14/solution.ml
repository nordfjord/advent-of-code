open Base
open Stdio
open Poly

let lines = In_channel.input_lines stdin |> List.map ~f:String.to_array |> List.to_array
let rows = Array.length lines
let cols = Array.length lines.(0)

let move_north grid =
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if grid.(r).(c) = 'O'
      then (
        let newr = ref r in
        while !newr > 0 && grid.(!newr - 1).(c) <> '#' && grid.(!newr - 1).(c) <> 'O' do
          newr := !newr - 1
        done;
        grid.(r).(c) <- '.';
        grid.(!newr).(c) <- 'O')
    done
  done;
  grid

let move_west grid =
  for c = 0 to cols - 1 do
    for r = 0 to rows - 1 do
      if grid.(r).(c) = 'O'
      then (
        let newc = ref c in
        while !newc > 0 && grid.(r).(!newc - 1) <> '#' && grid.(r).(!newc - 1) <> 'O' do
          newc := !newc - 1
        done;
        grid.(r).(c) <- '.';
        grid.(r).(!newc) <- 'O')
    done
  done;
  grid

let move_south grid =
  for r = rows - 1 downto 0 do
    for c = 0 to cols - 1 do
      if grid.(r).(c) = 'O'
      then (
        let newr = ref r in
        while
          !newr < rows - 1 && grid.(!newr + 1).(c) <> '#' && grid.(!newr + 1).(c) <> 'O'
        do
          newr := !newr + 1
        done;
        grid.(r).(c) <- '.';
        grid.(!newr).(c) <- 'O')
    done
  done;
  grid

let move_east grid =
  for c = cols - 1 downto 0 do
    for r = 0 to rows - 1 do
      if grid.(r).(c) = 'O'
      then (
        let newc = ref c in
        while
          !newc < cols - 1 && grid.(r).(!newc + 1) <> '#' && grid.(r).(!newc + 1) <> 'O'
        do
          newc := !newc + 1
        done;
        grid.(r).(c) <- '.';
        grid.(r).(!newc) <- 'O')
    done
  done;
  grid

let cycle_all coords =
  Array.copy_matrix coords |> move_north |> move_west |> move_south |> move_east

let score grid =
  Array.foldi grid ~init:0 ~f:(fun r acc line ->
    let count = Array.count line ~f:(Char.equal 'O') in
    let score = count * (rows - r) in
    acc + score)

(* part 1 *)
let () = Array.copy_matrix lines |> move_north |> score |> printf "%d\n"

let find_cycle grid =
  let seen = Hashtbl.Poly.create () in
  let rev = Hashtbl.Poly.create () in
  let rec aux i grid =
    let grid = cycle_all grid in
    if Hashtbl.mem seen grid
    then (
      let first_occurrence = Hashtbl.find_exn seen grid in
      let cycle_length = i - first_occurrence in
      let total_cycles = 1_000_000_000 - first_occurrence in
      let cycles = total_cycles % cycle_length in
      Hashtbl.find_exn rev (cycles + first_occurrence))
    else (
      Hashtbl.add_exn seen ~key:grid ~data:i;
      Hashtbl.add_exn rev ~key:i ~data:grid;
      aux (i + 1) grid)
  in
  aux 1 grid

(* part 2 *)
let () = find_cycle lines |> score |> printf "%d\n"
