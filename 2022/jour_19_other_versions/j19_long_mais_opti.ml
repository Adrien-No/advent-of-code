type blueprint = {
  core : int; (* cost of ore *)
  cclay : int;
  cobsi : int*int; (* (ore, clay) *)
  cgeode : int*int; (* (ore, obsi) *)
}

type ressources = Ore | Clay | Obsi | Geode
type choix = Construire of ressources | Economiser

type choix_coups = {
  mutable eco : bool;
  mutable ore : bool; (* veut-on tester le choix où l'on crée un robot ore ? *)
  mutable clay : bool;
  mutable obsi : bool;
  mutable geode : bool;
}

let minutes_max = 24
(* ================================================================ entrée ================================================================ *)
let data =
  let f = Scanf.Scanning.from_channel (open_in "j19.txt") in
  let rec read l =
    try
      read (Scanf.bscanf f "Blueprint %i: Each ore robot costs %i ore. Each clay robot costs %i ore. Each obsidian robot costs %i ore and %i clay. Each geode robot costs %i ore and %i obsidian.\n"
              (fun id ore_cost clay_cost obsi_cost_0 obsi_cost_1 geode_cost_0 geode_cost_1 ->
                 (id,{core= ore_cost ; cclay = clay_cost ; cobsi = (obsi_cost_0, obsi_cost_1) ; cgeode = (geode_cost_0, geode_cost_1)})) :: l)
    with End_of_file -> l
  in
  read []
(* ======================================================================================================================================== *)

(* ============================================================== affichage =============================================================== *)
let print_inv (a,b,c,d) =
  Printf.printf "(n_ore = %i, n_clay = %i, n_obsi = %i, n_geode = %i)\n" a b c d

let print_base (a,b,c,d) =
  Printf.printf "(r_ore = %i, r_clay = %i, r_obsi = %i, r_geode = %i)\n" a b c d
(* ======================================================================================================================================== *)

let can_craft (a,b,c,d) (bp:blueprint) (r:ressources) : bool =
  (* Printf.printf "On a :"; *)
  (* print_inv (a,b,c,d); *)
  (* print_newline(); *)
  match r with
    Ore ->  a >= bp.core
  | Clay -> a >= bp.cclay
  | Obsi -> a >= fst bp.cobsi && b >= snd bp.cobsi
  | Geode -> a >= fst bp.cgeode && c >= snd bp.cgeode

exception Trouve of bool array
let max_geode (bp:blueprint) : int =

  let max_ore = List.fold_left max 0 [(*bp.core;*)bp.cclay;fst bp.cobsi;fst bp.cgeode] in
  let max_clay = snd bp.cobsi in
  let max_obsi = snd bp.cgeode in
  let max_found = [|0;0;0;0;0;0;0;0|] in (* n_ore, n_ *)
  let update_max_found (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) =
    let n = [|n_ore; n_clay; n_obsi; n_geode; r_ore; r_clay; r_obsi; r_geode|] in
    Array.blit (Array.map2 max n max_found) 0 max_found 0 (Array.length max_found)
  in
  let rec aux_naive (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) (minute:int) (choix:choix): int =
    let inv,base =
      (* on compte la récolte ainsi que le choix pris pour les nouvelles valeurs de inv et base *)
      match choix with
        Economiser -> (*Printf.printf "Economiser\n";*)(n_ore+r_ore,n_clay+r_clay,n_obsi+r_obsi,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode)
      | Construire ressource ->
        match ressource with
          Ore -> (*Printf.printf "Construire Ore\n";*)(n_ore+r_ore-bp.core            ,n_clay+r_clay             ,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore+1,r_clay,r_obsi,r_geode)

        | Clay -> (* Printf.printf "Construire Clay\n"; *)(n_ore+r_ore-bp.cclay         ,n_clay+r_clay             ,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore,r_clay+1,r_obsi,r_geode)

        | Obsi -> (* Printf.printf "Construire Obsi\n"; *)(n_ore+r_ore-fst bp.cobsi     ,n_clay+r_clay-snd bp.cobsi,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore,r_clay,r_obsi+1,r_geode)

        | Geode -> (* Printf.printf "Construire Geode\n"; *)(n_ore+r_ore- fst bp.cgeode ,n_clay+r_clay             ,n_obsi+r_obsi - snd bp.cgeode,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode+1)

    in
    let (n_ore, n_clay, n_obsi, n_geode), (r_ore, r_clay, r_obsi, r_geode) = inv, base in

    (*Printf.printf "minute : %i\n" minute;
      print_inv inv;
      print_base base;*)
    if minute > minutes_max then
      0 (* on peut pas faire pire *)
    else if minute = minutes_max || not (Array.exists2 (>=) max_found [|n_ore; n_clay; n_obsi; n_geode; r_ore; r_clay; r_obsi; r_geode|]) then
      n_geode
    else
      let coups = [|true;true;true;true;true|] in (* les cases correspondent à Economiser, Construire (Ore, Clay, Obsi et Geode) *)

try

  if can_craft inv bp Ore  |> not || r_ore >= max_ore then
    coups.(1) <- false;
  if can_craft inv bp Clay |> not || r_clay >= max_clay then
    coups.(2) <- false;
  if can_craft inv bp Obsi |> not || r_obsi >= max_obsi then
    coups.(3) <- false;
  if not (can_craft inv bp Geode) then
    coups.(4) <- false;
  (* on fait des Geodes dès qu'on peut *)
  if can_craft inv bp Geode then
    begin
      for i = 0 to 3 do coups.(0) <- false ; done;
        raise (Trouve coups);
    end

  (* build all robot Ore first *)
  else if r_ore < max_ore then
    if can_craft inv bp Ore then
      raise (Trouve [|false;true;false;false;false|])
    else
      if can_craft inv bp Clay then
        raise (Trouve [|false;true;true;false;false|])
      else
        raise (Trouve [|true;false;false;false;false|])

  else if can_craft inv bp Obsi && r_obsi < max_obsi then
    raise (Trouve [|false;false;false;true;false|])
  else if can_craft inv bp Clay && r_clay < max_clay then
    raise (Trouve [|false;false;true;false;false|])
  else
    raise (Trouve coups)

with Trouve coups ->
begin
  update_max_found inv base;
  [Economiser ; Construire Ore ; Construire Clay ; Construire Obsi ; Construire Geode]
  |> List.filteri (fun i _ -> coups.(i))
  |> List.map (fun action -> aux_naive inv base (minute+1) action)
  |> List.fold_left max 0
end
in
(* le compteur "minute" est décalé de "un" par rapport à l'énoncé car nous on commence avec 1 ore par le robot (pour cette implémentation) *)
aux_naive (0, 0, 0, 0)
  (1, 0, 0, 0)
  1
  (Economiser)

let compute_output (l : (int*blueprint) list) : int =
  l
  |> List.map (fun (id,bp) -> let m = max_geode bp in Printf.printf "id : %i | max : %i\n" id m ; id * m)
  |> List.fold_left (+) 0

let _ =
  Printf.printf "Partie 1 : %i\n" (compute_output data)

(* 1685 : too low *)

(* 1708 : too low (4m8) *)

(* pour deux entrées : plus de 23mins *)
