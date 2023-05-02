type blueprint = {
  core : int; (* cost of ore *)
  cclay : int;
  cobsi : int*int; (* (ore, clay) *)
  cgeode : int*int; (* (ore, obsi) *)
}

type ressources = Ore | Clay | Obsi | Geode
type choix = Construire of ressources | Economiser

let minutes_max = 17
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
      if minute > minutes_max || n_ore < 0 || n_clay < 0 || n_obsi < 0 then
        0 (* on peut pas faire pire *)
      else if minute = minutes_max then
        n_geode
      else
        (*on va faire une série de tests pour supprimer des branches.*)

        (* on peut toujours faire un constructeur de geodes *)
        if r_ore >= fst bp.cgeode && r_obsi >= snd bp.cgeode then
          aux_naive inv base (minute+1) (Construire Geode)

        (* on a assez d'obsidienne => il manque de l'ore *)
        else if r_obsi >= snd bp.cgeode then
          (* /\ améliorable (conséquences sur les coups suivants si on skip ou non) *)
          max (aux_naive inv base (minute+1) (Economiser)) (aux_naive inv base (minute+1) (Construire Ore))
        else
          (* Il nous manque des obsis ou/et des ore *)

          (* /\ peut-etre faux /\ on peut toujours créer un constructeur d'obsidiennes *)
          if r_clay >= bp.cclay then
            (* on a qu'à attendre pour l'obsi (on peut pas faire mieux) *)
            (* on Eco pas car au pire on peut faire un constructeur d'obsi () *)
            [Construire Obsi ; Construire Ore ; Construire Geode] |> List.map (fun action -> aux_naive inv base (minute+1) action) |> List.fold_left max 0


      (* on a assez de core *)
      else if r_ore >= fst bp.cgeode && r_ore >= bp.cclay && r_ore >= fst bp.cobsi then


          else
          [Economiser ; Construire Clay ; Construire Obsi ; Construire Ore ; Construire Geode]
          |> List.map (fun action -> aux_naive inv base (minute+1) action)
          |> List.fold_left max 0

  in
  (* le compteur "minute" est décalé de "un" par rapport à l'énoncé car nous on commence avec 1 ore par le robot (pour cette implémentation) *)
  aux_naive (0, 0, 0, 0)
    (1, 0, 0, 0)
    0
    (Economiser)

let compute_output (l : (int*blueprint) list) : int =
  l
  |> List.map (fun (id,bp) -> id * max_geode bp)
  |> List.fold_left (+) 0

let _ =
  Printf.printf "Partie 1 : %i\n" (compute_output data)
