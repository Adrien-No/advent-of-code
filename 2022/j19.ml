type blueprint = {
  ore_cost : int;
  clay_cost : int;
  obsi_cost : int*int; (* (ore, clay) *)
  geode_cost : int*int; (* (ore, obsi) *)
}

type inventory = {
  mutable ore_nb : int;
  mutable clay_nb : int;
  mutable obsi_nb : int;
  mutable geode_nb : int;
}

type base = {
  mutable ore_robot : int;
  mutable clay_robot : int;
  mutable obsi_robot : int;
  mutable geode_robot : int;
}

type ressources = Ore | Clay | Obsi | Geode
type choix = Construire of ressources | Economiser

let minutes_max = 3
(* ================================================================ entrée ================================================================ *)
let data =
  let f = Scanf.Scanning.from_channel (open_in "j19.txt") in
  let rec read l =
    try
      read (Scanf.bscanf f "Blueprint %i: Each ore robot costs %i ore. Each clay robot costs %i ore. Each obsidian robot costs %i ore and %i clay. Each geode robot costs %i ore and %i obsidian.\n"
              (fun id ore_cost clay_cost obsi_cost_0 obsi_cost_1 geode_cost_0 geode_cost_1 ->
                 (id,{ore_cost = ore_cost ; clay_cost = clay_cost ; obsi_cost = (obsi_cost_0, obsi_cost_1) ; geode_cost = (geode_cost_0, geode_cost_1)})) :: l)
    with End_of_file -> l
  in
  read []
(* ======================================================================================================================================== *)

(* ============================================================== affichage =============================================================== *)
let print_inv inv =
  Printf.printf "(ore_nb = %i, clay_nb = %i, obsi_nb = %i, geode_nb = %i)\n" inv.ore_nb inv.clay_nb inv.obsi_nb inv.geode_nb

let print_base b =
  Printf.printf "(ore_robot = %i, clay_robot = %i, obsi_robot = %i, geode_robot = %i)\n" b.ore_robot b.clay_robot b.obsi_robot b.geode_robot
(* ======================================================================================================================================== *)

let max_geode (bp:blueprint) : int =
  let rec aux_naive (inv:inventory) (base:base) (minute:int) (choix:choix): int =
    (
      match choix with
        Economiser -> ()
      | Construire ressource ->
        match ressource with
          Ore -> inv.ore_nb <- inv.ore_nb - bp.ore_cost; base.ore_robot <- base.ore_robot + 1
        | Clay -> inv.ore_nb <- inv.ore_nb - bp.clay_cost; base.clay_robot <- base.clay_robot + 1
        | Obsi ->
          inv.ore_nb <- inv.ore_nb - fst bp.obsi_cost;
          inv.clay_nb <- inv.clay_nb - snd bp.obsi_cost;
          base.obsi_robot <- base.obsi_robot + 1
        | Geode ->
          inv.ore_nb <- inv.ore_nb - fst bp.geode_cost;
          inv.obsi_nb <- inv.obsi_nb - snd bp.geode_cost;
          base.geode_robot <- base.geode_robot + 1
    );

      if minute < 0 || inv.ore_nb < 0 || inv.clay_nb < 0 || inv.obsi_nb < 0 then
        0 (* on peut pas faire pire *)
      else if minute = 0 then
        inv.geode_nb
      else
        (* récolte *)
        let inv = {ore_nb = inv.ore_nb + base.ore_robot ; clay_nb = inv.clay_nb + base.clay_robot ; obsi_nb = inv.obsi_nb + base.obsi_robot ; geode_nb = inv.geode_nb + base.geode_robot} in
        Printf.printf "=== Minute %i ===\n" (minutes_max-minute+1);
        print_inv inv;
        print_base base;
        (* on iter sur les choix possibles *)
        [Economiser ; Construire Ore ; Construire Clay ; Construire Obsi ; Construire Geode]
        |> List.map (fun action -> aux_naive inv base (minute-1) action)
        |> List.fold_left max 0
  in
  (* le compteur "minute" est décalé de "un" par rapport à l'énoncé car nous on commence avec 1 ore par le robot (pour cette implémentation) *)
  aux_naive {ore_nb = 0 ; clay_nb = 0 ; obsi_nb = 0 ; geode_nb = 0}
    {ore_robot = 1 ; clay_robot = 0 ; obsi_robot = 0 ; geode_robot = 0}
    minutes_max
    (Economiser)

let compute_output (l : (int*blueprint) list) : int =
  l
  |> List.map (fun (id,bp) -> id * max_geode bp)
  |> List.fold_left (+) 0

let _ =
  Printf.printf "Partie 1 : %i\n" (compute_output data)
