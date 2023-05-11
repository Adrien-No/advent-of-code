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

type maxs_domaine = {
  mutable ore : int;
  mutable clay : int;
  mutable obsi : int;
  mutable geode : int;

  mutable r_ore : int;
  mutable r_clay : int;
  mutable r_obsi : int;
  mutable r_geode : int;
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
  (*Printf.printf "On a :";*)
  (*print_inv (a,b,c,d);*)
  (*print_newline();*)
  match r with
    Ore ->  a >= bp.core
  | Clay -> a >= bp.cclay
  | Obsi -> a >= fst bp.cobsi && b >= snd bp.cobsi
  | Geode -> a >= fst bp.cgeode && c >= snd bp.cgeode

let max_geode (bp:blueprint) : int =

  (* max_domaine *)
  let (md:maxs_domaine) = {ore = (-1);clay=(-1);obsi=(-1);geode=(-1);r_ore=(-1);r_clay=(-1);r_obsi=(-1);r_geode=(-1) } in
  let rec aux_naive (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) (minute:int) (choix:choix): int =
    let inv,base =
      (* on compte la récolte ainsi que le choix pris pour les nouvelles valeurs de inv et base *)
      match choix with
        Economiser -> (n_ore+r_ore,n_clay+r_clay,n_obsi+r_obsi,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode)
      | Construire ressource ->
        match ressource with
          Ore -> (n_ore+r_ore-bp.core,n_clay+r_clay,n_obsi+r_obsi,n_geode+r_geode), (r_ore+1,r_clay,r_obsi,r_geode)

        | Clay -> (n_ore+r_ore-bp.core,n_clay+r_clay,n_obsi+r_obsi,n_geode+r_geode), (r_ore,r_clay+1,r_obsi,r_geode)

        | Obsi -> (n_ore+r_ore-fst bp.cobsi,n_clay+r_clay-snd bp.cobsi,n_obsi+r_obsi,n_geode+r_geode), (r_ore,r_clay,r_obsi+1,r_geode)

        | Geode -> (n_ore+r_ore- fst bp.cgeode ,n_clay+r_clay,n_obsi+r_obsi - snd bp.cgeode,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode+1)

    in
    let (n_ore, n_clay, n_obsi, n_geode), (r_ore, r_clay, r_obsi, r_geode) = inv, base in

    (* Printf.printf "minute : %i\n" minute; *)
    (* print_inv inv; *)
    (* print_base base; *)
    if minute > minutes_max || (n_ore < md.ore && n_clay < md.clay && n_obsi < md.obsi && n_geode < md.geode && r_ore < md.r_ore && r_clay < md.r_clay && r_obsi < md.obsi && r_geode < md.geode) then
      (* le dernier "grand ou" correspond au test de domaine max*)
      0 (* on peut pas faire pire *)
    else if minute = minutes_max then
      n_geode
    else (
      let coups_possibles = {eco=true ; ore=true ; clay=true ; obsi=true ; geode=true} in

      (*on va faire une série de tests pour supprimer des branches.*)

      (* on peut faire un geode || on peut toujours faire un constructeur de geodes *)
      if can_craft inv bp Geode ||r_ore >= fst bp.cgeode && r_obsi >= snd bp.cgeode then (
        aux_naive inv base (minute+1) (Construire Geode)
      )
      else (
        (* /\ peut-etre faux /\ on peut toujours créer un constructeur d'obsidiennes *)
        if r_clay >= bp.cclay then (
          (* on a qu'à attendre pour la clay de l'obsi (on peut pas faire mieux) *)
          (* on Eco pas car au pire on peut faire un constructeur d'obsi () *)

          (* ----------------- ces deux réduisent drastiquement la complexité --------------------------- *)
          coups_possibles.eco <- false;
          coups_possibles.clay <- false;
          (* -------------------------------------------------------------------------------------------- *)
        );
        (* on a assez de ore (chaque tour on gagne assez pour créer n'importe quel constructeur)*)
        if r_ore >= fst bp.cgeode && r_ore >= bp.cclay && r_ore >= fst bp.cobsi then
          coups_possibles.ore <- false;
        (* /\ à vérifier *)
        if can_craft inv bp Ore && can_craft inv bp Clay && can_craft inv bp Obsi && can_craft inv bp Geode then
          coups_possibles.eco <- false;

        md.ore <- max md.ore n_ore;
        md.clay <- max md.clay n_clay;
        md.obsi <- max md.obsi n_obsi;
        md.geode <- max md.geode n_geode;

        md.ore <- max md.ore r_ore;
        md.clay <- max md.clay r_clay;
        md.obsi <- max md.obsi r_obsi;
        md.geode <- max md.geode r_geode;

        (* maintenant on iter uniquement sur les coups pas considérés comme inutiles *)
        let cp = coups_possibles in
        (if cp.eco then [Economiser] else [])::(if cp.ore && can_craft inv bp Ore then [Construire Ore] else [])::(if cp.clay && can_craft inv bp Clay then [Construire Clay] else [])::(if cp.obsi && can_craft inv bp Obsi then [Construire Obsi] else [])::(if cp.geode && can_craft inv bp Geode then [Construire Geode] else [])::[]
        |> List.concat
        |> List.map (fun action -> aux_naive inv base (minute+1) action)
        |> List.fold_left max 0
      )
    )
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
