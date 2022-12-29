open Printf

let t = Sys.time ()

module Ore = struct
  type t = {ore: int; clay: int; obsidian: int; geode: int}

  let concat a b =
    { ore= a.ore + b.ore
    ; clay= a.clay + b.clay
    ; obsidian= a.obsidian + b.obsidian
    ; geode= a.geode + b.geode }

  let sub a b =
    { ore= a.ore - b.ore
    ; clay= a.clay - b.clay
    ; obsidian= a.obsidian - b.obsidian
    ; geode= a.geode - b.geode }

  let gt a b =
    a.ore >= b.ore && a.clay >= b.clay && a.obsidian >= b.obsidian
    && a.geode >= b.geode

  let empty = {ore= 0; clay= 0; obsidian= 0; geode= 0}
end

type blueprint =
  { id: int
  ; ore_cost: Ore.t
  ; clay_cost: Ore.t
  ; obsidian_cost: Ore.t
  ; geode_cost: Ore.t
  ; max_ore: int
  ; max_clay: int
  ; max_obsidian: int }

let parse_blueprint s =
  Scanf.sscanf s
    "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. \
     Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d \
     ore and %d obsidian."
    (fun id ore clay_ore obsidian_ore obsidian_clay geode_ore geode_obsidian ->
      { id
      ; ore_cost= {Ore.empty with ore}
      ; clay_cost= {Ore.empty with ore= clay_ore}
      ; obsidian_cost= {Ore.empty with ore= obsidian_ore; clay= obsidian_clay}
      ; geode_cost= {Ore.empty with ore= geode_ore; obsidian= geode_obsidian}
      ; max_ore= max clay_ore (max obsidian_ore geode_ore)
      ; max_clay= obsidian_clay
      ; max_obsidian= geode_obsidian } )

let blueprints =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None )
  |> Seq.map parse_blueprint |> List.of_seq

type action =
  | BuildOreRobot
  | BuildClayRobot
  | BuildObsidianRobot
  | BuildGeodeRobot

type state = {robots: Ore.t; resources: Ore.t; minutes_left: int}

let actions =
  [BuildOreRobot; BuildClayRobot; BuildObsidianRobot; BuildGeodeRobot]

let can_perform (bp : blueprint) state action =
  match action with
  | BuildOreRobot ->
      Ore.gt state.resources bp.ore_cost
  | BuildClayRobot ->
      Ore.gt state.resources bp.clay_cost
  | BuildObsidianRobot ->
      Ore.gt state.resources bp.obsidian_cost
  | BuildGeodeRobot ->
      Ore.gt state.resources bp.geode_cost

let initial =
  { robots= {ore= 1; clay= 0; obsidian= 0; geode= 0}
  ; resources= {ore= 0; clay= 0; obsidian= 0; geode= 0}
  ; minutes_left= 24 }

let tick state =
  { state with
    resources= Ore.concat state.resources state.robots
  ; minutes_left= state.minutes_left - 1 }

let evolve (blueprint : blueprint) state action =
  let state = tick state in
  match action with
  | BuildOreRobot ->
      { state with
        resources= Ore.sub state.resources blueprint.ore_cost
      ; robots= {state.robots with ore= state.robots.ore + 1} }
  | BuildClayRobot ->
      { state with
        resources= Ore.sub state.resources blueprint.clay_cost
      ; robots= {state.robots with clay= state.robots.clay + 1} }
  | BuildObsidianRobot ->
      { state with
        resources= Ore.sub state.resources blueprint.obsidian_cost
      ; robots= {state.robots with obsidian= state.robots.obsidian + 1} }
  | BuildGeodeRobot ->
      { state with
        resources= Ore.sub state.resources blueprint.geode_cost
      ; robots= {state.robots with geode= state.robots.geode + 1} }

let potential_production state =
  let bots = state.robots.geode in
  let upper = bots + state.minutes_left in
  state.resources.geode + ((upper - bots) * (upper + bots) / 2)

let tick_until_can_perform bp state action =
  let state = ref state in
  while (not (can_perform bp !state action)) && !state.minutes_left > 1 do
    state := tick !state
  done ;
  evolve bp !state action

let next_states bp state =
  let next_actions =
    actions
    |> List.filter_map (fun action ->
           if action = BuildOreRobot && state.robots.ore >= bp.max_ore then None
             (* We don't want more clay than necessary *)
           else if action = BuildClayRobot && state.robots.clay >= bp.max_clay
           then None (* We don't want more obsidian than necessary *)
           else if
             action = BuildObsidianRobot
             && state.robots.obsidian >= bp.max_obsidian
           then None
           else tick_until_can_perform bp state action |> Option.some )
  in
  match next_actions with [] -> [tick state] | actions -> actions

(* Modified DFS *)
let rec simulate (blueprint : blueprint) max_geodes state =
  max_geodes := max !max_geodes state.resources.geode ;
  if state.minutes_left = 0 then ()
  else if potential_production state <= !max_geodes then ()
  else next_states blueprint state |> List.iter (simulate blueprint max_geodes)

let part1 () =
  blueprints
  |> List.map (fun bp ->
         let max_geodes = ref 0 in
         simulate bp max_geodes initial ;
         (bp, !max_geodes) )
  |> List.fold_left (fun sum (bp, max) -> sum + (bp.id * max)) 0
  |> printf "Part 1: %d\n"

let part2 () =
  blueprints |> List.to_seq |> Seq.take 3 |> List.of_seq
  |> List.map (fun bp ->
         let max_geodes = ref 0 in
         let state = {initial with minutes_left= 32} in
         simulate bp max_geodes state ;
         !max_geodes )
  |> List.fold_left (fun sum max -> sum * max) 1
  |> printf "Part 2: %d\n"

let () =
  part1 () ;
  part2 () ;
  printf "Runtime: %fms\n" ((1000. *. Sys.time ()) -. t)
