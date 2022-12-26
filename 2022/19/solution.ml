open Printf

module Ore = struct
  type t = { ore : int; clay : int; obsidian : int; geode : int }
  [@@deriving show]

  let concat a b =
    {
      ore = a.ore + b.ore;
      clay = a.clay + b.clay;
      obsidian = a.obsidian + b.obsidian;
      geode = a.geode + b.geode;
    }

  let sub a b =
    {
      ore = a.ore - b.ore;
      clay = a.clay - b.clay;
      obsidian = a.obsidian - b.obsidian;
      geode = a.geode - b.geode;
    }

  let gt a b =
    a.ore >= b.ore && a.clay >= b.clay && a.obsidian >= b.obsidian
    && a.geode >= b.geode

  let empty = { ore = 0; clay = 0; obsidian = 0; geode = 0 }
end

type blueprint = {
  id : int;
  ore_cost : Ore.t;
  clay_cost : Ore.t;
  obsidian_cost : Ore.t;
  geode_cost : Ore.t;
  max_ore : int;
  max_clay : int;
  max_obsidian : int;
}

let parse_blueprint s =
  Scanf.sscanf s
    "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. \
     Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d \
     ore and %d obsidian."
    (fun id ore clay_ore obsidian_ore obsidian_clay geode_ore geode_obsidian ->
      {
        id;
        ore_cost = { Ore.empty with ore };
        clay_cost = { Ore.empty with ore = clay_ore };
        obsidian_cost =
          { Ore.empty with ore = obsidian_ore; clay = obsidian_clay };
        geode_cost =
          { Ore.empty with ore = geode_ore; obsidian = geode_obsidian };
        max_ore = max clay_ore (max obsidian_ore geode_ore);
        max_clay = obsidian_clay;
        max_obsidian = geode_obsidian;
      })

let blueprints =
  Seq.of_dispenser (fun _ ->
      match read_line () with x -> Some x | exception End_of_file -> None)
  |> Seq.map parse_blueprint |> List.of_seq

type action =
  | DoNothing
  | BuildOreRobot
  | BuildClayRobot
  | BuildObsidianRobot
  | BuildGeodeRobot
[@@deriving show]

type state = { robots : Ore.t; resources : Ore.t; minutes_left : int }
[@@deriving show]

let actions =
  [
    BuildOreRobot;
    BuildClayRobot;
    BuildObsidianRobot;
    BuildGeodeRobot;
    DoNothing;
  ]

let can_perform (bp : blueprint) state action =
  match action with
  | BuildOreRobot -> Ore.gt state.resources bp.ore_cost
  | BuildClayRobot -> Ore.gt state.resources bp.clay_cost
  | BuildObsidianRobot -> Ore.gt state.resources bp.obsidian_cost
  | BuildGeodeRobot -> Ore.gt state.resources bp.geode_cost
  | DoNothing -> true

let initial =
  {
    robots = { ore = 1; clay = 0; obsidian = 0; geode = 0 };
    resources = { ore = 0; clay = 0; obsidian = 0; geode = 0 };
    minutes_left = 24;
  }

let evolve (blueprint : blueprint) state action =
  let state =
    {
      state with
      resources = Ore.concat state.resources state.robots;
      minutes_left = state.minutes_left - 1;
    }
  in

  match action with
  | DoNothing -> state
  | BuildOreRobot ->
      {
        state with
        resources = Ore.sub state.resources blueprint.ore_cost;
        robots = { state.robots with ore = state.robots.ore + 1 };
      }
  | BuildClayRobot ->
      {
        state with
        resources = Ore.sub state.resources blueprint.clay_cost;
        robots = { state.robots with clay = state.robots.clay + 1 };
      }
  | BuildObsidianRobot ->
      {
        state with
        resources = Ore.sub state.resources blueprint.obsidian_cost;
        robots = { state.robots with obsidian = state.robots.obsidian + 1 };
      }
  | BuildGeodeRobot ->
      {
        state with
        resources = Ore.sub state.resources blueprint.geode_cost;
        robots = { state.robots with geode = state.robots.geode + 1 };
      }

module T = Domainslib.Task

(* This task is about figuring out the most geode we can get with a blueprint

   One way to solve this is to find the "shortest path" to a geode robot and repeat the actions

   necessary for that *)

let potential_production state =
  let bots = state.robots.geode in
  let upper = bots + state.minutes_left in
  state.resources.geode + ((upper - bots) * (upper + bots) / 2)

let can_produce_geode_robot_per_minute bp state =
  bp.geode_cost.ore <= state.robots.ore
  && bp.geode_cost.obsidian <= state.robots.obsidian

(* Modified DFS *)
let rec simulate (blueprint : blueprint) state max_geodes action =
  if state.minutes_left = 0 then
    max_geodes := max !max_geodes state.resources.geode
    (* Prune branches of the DFS
       We don't want to have more ore capacity than needed to build a geode bot every minute *)
  else if action = BuildOreRobot && state.robots.ore >= blueprint.max_ore then
    () (* We don't want more clay than necessary *)
  else if action = BuildClayRobot && state.robots.clay >= blueprint.max_clay
  then () (* We don't want more obsidian than necessary *)
  else if
    action = BuildObsidianRobot
    && state.robots.obsidian >= blueprint.max_obsidian
  then ()
    (* This branch doesn't have a hope in hell of outperforming the best we've found *)
  else if potential_production state <= !max_geodes then ()
    (* If we can build a geode robot then that's the only thing that makes sense to build *)
  else
    let next_state = evolve blueprint state action in
    actions
    |> List.filter (can_perform blueprint next_state)
    |> List.iter (simulate blueprint next_state max_geodes)

let part1 pool =
  T.async pool (fun _ ->
      blueprints
      |> List.map (fun bp ->
             T.async pool (fun _ ->
                 printf "Blueprint %d\n" bp.id;
                 let max_geodes = ref 0 in
                 actions
                 |> List.filter (can_perform bp initial)
                 |> List.iter (simulate bp initial max_geodes);
                 printf "blueprint=%d; result=%d quality_level=%d\n%!" bp.id
                   !max_geodes (bp.id * !max_geodes);
                 (bp, !max_geodes)))
      |> List.map (T.await pool)
      |> List.fold_left (fun sum (bp, max) -> sum + (bp.id * max)) 0
      |> printf "Part 1: %d\n")

let part2 pool =
  T.async pool (fun _ ->
      blueprints |> List.to_seq |> Seq.take 3 |> List.of_seq
      |> List.map (fun bp ->
             T.async pool (fun _ ->
                 printf "Blueprint %d\n" bp.id;
                 let max_geodes = ref 0 in
                 let state = { initial with minutes_left = 32 } in
                 actions
                 |> List.filter (can_perform bp state)
                 |> List.iter (simulate bp state max_geodes);
                 printf "blueprint=%d; result=%d quality_level=%d\n%!" bp.id
                   !max_geodes (bp.id * !max_geodes);
                 !max_geodes))
      |> List.map (T.await pool)
      |> List.fold_left (fun sum max -> sum * max) 1
      |> printf "Part 2: %d\n")

let () =
  let pool = T.setup_pool ~num_domains:8 () in
  T.run pool (fun _ ->
      let p1 = part1 pool in
      let p2 = part2 pool in
      T.await pool p1;
      T.await pool p2);
  T.teardown_pool pool
