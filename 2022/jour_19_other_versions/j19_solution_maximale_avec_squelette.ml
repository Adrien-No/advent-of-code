type blueprint = {
  core : int; (* cost of ore *)
  cclay : int;
  cobsi : int*int; (* (ore, clay) *)
  cgeode : int*int; (* (ore, obsi) *)
}
type four_uplet = int * int * int * int
type ressources = Ore | Clay | Obsi | Geode
type choix = Construire of ressources | Economiser

(* k choosed for k-nearest-neigthboors *)
let k1 = 620
let k2 = 3150
(* our epsilons *)
let eps_rgeode = 100
let eps_ngeode = 10

let eps_robsi = 1 (*30*)
let eps_nobsi = 1  (*5*)
let eps_rclay = 0
let eps_nclay = 0
let eps_rore =  0
let eps_nore = 0

let comparePath = (fun (_,(nore1,nclay1,nobsi1,ngeode1),(rore1,rclay1,robsi1,rgeode1)) (_,(nore2,nclay2,nobsi2,ngeode2),(rore2,rclay2,robsi2,rgeode2)) ->
    eps_nore  * (compare nore1 nore2)   +
    eps_rore  * (compare rore1 rore2)   +
    eps_nclay * (compare nclay1 nclay2) +
    eps_rclay * (compare rclay1 rclay2) +
    eps_nobsi * (compare nobsi1 nobsi2) +
    eps_robsi * (compare robsi1 robsi2) +
    eps_ngeode * (compare ngeode1 ngeode2) +
    eps_rgeode * (compare rgeode1 rgeode2)
  )

let get_k_firsts l k =
  let rec aux l acc k =
    match l with
      [] -> acc
    | t::q -> if k = 0 then t::acc else aux q (t::acc) (k-1)
  in
  aux l [] k

let map_rec_tale f l =
  let rec aux acc = function
      [] -> acc
    | t::q -> let r = f t in aux (r::acc) q
  in
  aux [] l

let concat_rec_tale l =
  let rec aux acc = function
      [] -> acc
    | t::q -> aux (t @ acc) q
  in
  aux [] l
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

let print_max_found t =
  Printf.printf "[|";
  Array.iteri (fun i x -> Printf.printf "%i" x; if not (i = Array.length t) then Printf.printf " ;"  ) t;
  Printf.printf "|]\n"

let string_of_choix c =
  match c with
    Economiser -> "Economiser"
  | Construire r -> "Construire " ^
                    match r with
                      Ore -> "Ore"
                    | Clay -> "Clay"
                    | Obsi -> "Obsi"
                    | Geode -> "Geode"

let print_choix_fouruplet_fouruplet_list (l:(choix*four_uplet*four_uplet) list) =
  Printf.printf "[";
  List.iteri (fun i (choix,(a,b,c,d),(e,f,g,h)) -> Printf.printf "(%s, (%i, %i, %i, %i), (%i, %i, %i, %i))" (string_of_choix choix) a b c d e f g h; if not (i = List.length l) then Printf.printf " ;\n") l;
  Printf.printf "]\n"
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
let max_geode (bp:blueprint) (minutes_max:int) k : int =

  let max_ore = List.fold_left max 0 [(*bp.core;*)bp.cclay;fst bp.cobsi;fst bp.cgeode] in
  let max_clay = snd bp.cobsi in
  let max_obsi = snd bp.cgeode in
  let max_found = [|0;0;0;0;0;0;0;0|] in (* n_ore, n_ *)
  let update_max_found (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) =
    let n = [|n_ore; n_clay; n_obsi; n_geode; r_ore; r_clay; r_obsi; r_geode|] in
    Array.blit (Array.map2 max n max_found) 0 max_found 0 (Array.length max_found)
    (* if max_found <> n then print_max_found max_found *)
  in

  (* pour suivre la première fois que la borne k est atteinte (ie on prend pas tout les candidats) *)
  let chip = ref true in
  (* ================================================================ "recurence" ================================================================ *)
  let max_n_plus_un (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) (minutes) (choix:choix): (choix*four_uplet*four_uplet) list =
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
    (* Printf.printf "minute : %i\n" minutes; print_inv inv; print_base base; *)
    (* if not (Array.exists2 (>=) max_found [|n_ore; n_clay; n_obsi; n_geode; r_ore; r_clay; r_obsi; r_geode|]) then *)
    (*   [] *)
    (* else *)
      let coups = [|true;true;true;true;true|] in (* les cases correspondent à Economiser, Construire (Ore, Clay, Obsi et Geode) *)
      try
        if can_craft inv bp Ore  |> not || r_ore >= max_ore then
          coups.(1) <- false;
        if can_craft inv bp Clay |> not || r_clay >= max_clay then
          coups.(2) <- false;
        if not (can_craft inv bp Obsi)  || r_obsi >= max_obsi then
          coups.(3) <- false;
        if not (can_craft inv bp Geode) then
          coups.(4) <- false;
        (* on fait des Geodes dès qu'on peut *)
        if can_craft inv bp Geode then
          begin
            (* Printf.printf " \n\nON PEUT CRAFT GEODE\n\n"; *)
            for i = 0 to 3 do coups.(0) <- false done;
            raise (Trouve coups);
          end
          (* ================= méthode FOMR (Full Ore then Max Resource) ============== *)
        (* else if r_ore < max_ore then *)
        (*   begin *)
        (*     if can_craft inv bp Ore then *)
        (*       raise (Trouve [|false;true;false;false;false|]) *)
        (*     else *)
        (*       raise (Trouve [|true;false;false;false;false|]) *)
        (*   end *)

        (* else if r_obsi < max_obsi && can_craft inv bp Obsi then *)
        (*       raise (Trouve [|false;false;false;true;false|]) *)
        (* else if r_clay < max_clay && can_craft inv bp Clay then *)
        (*       raise (Trouve [|false;true;false;false;false|]) *)

  (* =============================== trop greedy ============================== *)
  (* build all robot Ore first *)
else (* if r_ore < max_ore then *)
  (*     if can_craft inv bp Ore then *)
  (*       raise (Trouve [|false;true;false;false;false|]) *)
  (*     else *)
  (*     if can_craft inv bp Clay then *)
  (*       raise (Trouve [|false;true;true;false;false|]) *)
  (*     else *)
  (*       raise (Trouve [|true;false;false;false;false|]) *)

  (* else if can_craft inv bp Obsi && r_obsi < max_obsi then *)
  (*   raise (Trouve [|false;false;false;true;false|]) *)
  (* else if can_craft inv bp Clay && r_clay < max_clay then *)
  (*   raise (Trouve [|false;false;true;false;false|]) *)
  (* else *)
  (* experimental *)
  (* if (can_craft inv bp Ore || n_ore = max_ore) && (can_craft inv bp Clay || n_clay = max_clay) && (can_craft inv bp Obsi || n_obsi = max_obsi) then *)
  (*   coups.(0) <- false; *)
  raise (Trouve coups)

(* ============================================================================ *)
with Trouve coups -> (
  update_max_found inv base;
  [(Economiser,inv,base) ; (Construire Ore,inv,base) ; (Construire Clay,inv,base) ; (Construire Obsi,inv,base) ; (Construire Geode,inv,base)]
  |> List.filteri (fun i _ -> coups.(i))
  (* |> (fun l -> let l2 = List.filteri (fun i _ -> coups.(i)) l in Printf.printf "len_avant_filtre :%i | apres :%i\n" (List.length l) (List.length l2); l2 ) *)
)
in
(* =========================================================================================================================================== *)

let knn = ref [(Economiser, (0,0,0,0), (1,0,0,0))] in
for minutes = 1 to minutes_max do
  (*print_choix_fouruplet_fouruplet_list !knn;*)
  (*if minutes = 24 then (Printf.printf "< Minute 24 > :";  print_max_found max_found );*)
  let suivants = concat_rec_tale (map_rec_tale (fun (move,inv,base) -> max_n_plus_un inv base minutes move) !knn) in
  let suiv_sorted = List.sort comparePath suivants |> List.rev in

  if !chip && List.length suiv_sorted >= k then
    ();(* (Printf.printf "< Minute %i > Borne des k atteinte !\n" minutes; (\*print_max_found max_found ;*\) chip := false); *)

  let k_firsts = get_k_firsts suiv_sorted k in
  knn := k_firsts;
done;
List.fold_left (fun b (_,(_,_,_,ngeode),_) -> max ngeode b) 0 !knn

let compute_output (l : (int*blueprint) list) : int =
  let minutes_max = 24 in
  l
  |> map_rec_tale (fun (id,bp) -> let m = max_geode bp minutes_max k1 in Printf.printf "id : %i | max : %i\n" id m ; id * m)
  |> List.fold_left (+) 0

let compute_output2 l : int =
  let minutes_max = 32 in
  let three_firsts = get_k_firsts (List.rev l) 2 in
  three_firsts
  |> map_rec_tale (fun (id,bp) -> let m = max_geode bp minutes_max k2 in Printf.printf "id : %i | max : %i\n" id m ; m)
  |> List.fold_left ( * ) 1

let _ =
  Printf.printf "Partie 1 : %i\n" (compute_output data);
  Printf.printf "Partie 2 : %i\n" (compute_output2 data);
  Printf.printf "k = %i\n" k2

(* solution partie 1 pour mon entrée : *)
(* 1958, trouvé avec k = 10_000 en 21,8 s *)
(* 1958, trouvé avec k = 1000 en 2,0 s*)
(* 1958, trouvé avec k = 620 en 1,3 s pour rgeode = 100, ngeode = 10, robsi = 1, nobsi = 1 et le reste = à 0. ça semble être a peu pres le k minimum *)

(* solution partie 2 pour mon entrée : *)

(* 10, 8, 43 *)
(* 11, 9, 43 : with k = 20000 *)
(* solution : 4257 pour k = 31500 (minimum) en 1,2 s*)
