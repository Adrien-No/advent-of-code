(* One game-lap is one minute, each minute we receive one resource per robot of this resource, *)
(* and each minute we can also do nothing or craft one robot of a resource *)

(* The differents resources are : *)
type resource = Ore | Clay | Obsi | Geode

(* So the different action/choices/choix that can be made are : *)
type choix = Construire of resource | Economiser

(* We want to explore the tree of possibilities given by choosing at each turn (or minute) an action to do. *)
(* Because 5^24 is a lot, we want to "cut" some branches of the tree, to reduce possibilities and find-out faster an optimal-one.*)
(* For this we'll use a bit of logical (function "max_n_plus_un") *)
(* And also realise a implement a beam-search algorithms *)

(* We'll call "inventory" (or inv) resource available at a given time. *)
(* "base" will be all the robots that we have at a given time.*)

(* representation of a blueprint (our entry) *)
type blueprint = {
  core : int; (* cost of ore *)
  cclay : int;
  cobsi : int*int; (* (ore, clay) *)
  cgeode : int*int; (* (ore, obsi) *)
}

 (* an inventory (inv) or a base *)
type four_uplet = int * int * int * int

(* constants chose for the beam search *)
let k1 = 620 (* that works for me, 1,3 s for the first entry with a laptop *)
let k2 = 3150 (* works also, in 1,2 s *)

(* our epsilons *)
let eps_rgeode = 100
let eps_ngeode = 10

let eps_robsi = 1 (*30*)
let eps_nobsi = 1  (*5*)
let eps_rclay = 0
let eps_nclay = 0
let eps_rore =  0
let eps_nore = 0

(* Compare two paths in the tree, according to epsilons. Will be used for sorting. *)
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

(* ================================================================ misc ================================================================ *)
let get_k_firsts l k =
  (* returns the k-first elements of the list l *)
  let rec aux l acc k =
    match l with
      [] -> acc
    | t::q -> if k = 0 then t::acc else aux q (t::acc) (k-1)
  in
  aux l [] k

let map_rec_tale f l =
  (* same as the map of stdlib but tail-recursive *)
  let rec aux acc = function
      [] -> acc
    | t::q -> let r = f t in aux (r::acc) q
  in
  aux [] l

let concat_rec_tale l =
  (* same as concat of stdlib but tail-recursive *)
  let rec aux acc = function
      [] -> acc
    | t::q -> aux (t @ acc) q
  in
  aux [] l

let can_craft (a,b,c,d) (bp:blueprint) (r:resource) : bool =
  (* returns true if and only if the resource r can be crafted with the inventory (a,b,c,d) *)
  match r with
    Ore ->  a >= bp.core
  | Clay -> a >= bp.cclay
  | Obsi -> a >= fst bp.cobsi && b >= snd bp.cobsi
  | Geode -> a >= fst bp.cgeode && c >= snd bp.cgeode

(* ====================================================================================================================================== *)

(* =============================================================== entry ================================================================ *)
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
(* ====================================================================================================================================== *)

(* ============================================================ main function =========================================================== *)
exception Trouve of bool array
let max_geode (bp:blueprint) (minutes_max:int) k : int =
  (* returns the maximum geode available after minutes_max mins, for the blueprint bp *)

  (* Since we can't create more than one robot at a time, it's useless to harvest more resources each turns that we could use on this turn. *)
  let max_ore = List.fold_left max 0 [bp.cclay;fst bp.cobsi;fst bp.cgeode] in
  let max_clay = snd bp.cobsi in
  let max_obsi = snd bp.cgeode in

  (* ================ "recurrence" ================ *)
  let max_n_plus_un (n_ore, n_clay, n_obsi, n_geode) (r_ore, r_clay, r_obsi, r_geode) (minutes) (choix:choix): (choix*four_uplet*four_uplet) list =
    let inv,base =
      (* we update inv and base according to the harvest and the choice we made (eventually spend the resources to build a robot) *)
      match choix with
        Economiser -> (n_ore+r_ore,n_clay+r_clay,n_obsi+r_obsi,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode)
      | Construire ressource ->
        match ressource with
          Ore -> (n_ore+r_ore-bp.core            ,n_clay+r_clay             ,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore+1,r_clay,r_obsi,r_geode)
        | Clay -> (n_ore+r_ore-bp.cclay         ,n_clay+r_clay             ,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore,r_clay+1,r_obsi,r_geode)
        | Obsi -> (n_ore+r_ore-fst bp.cobsi     ,n_clay+r_clay-snd bp.cobsi,n_obsi+r_obsi                ,n_geode+r_geode), (r_ore,r_clay,r_obsi+1,r_geode)
        | Geode -> (n_ore+r_ore- fst bp.cgeode ,n_clay+r_clay             ,n_obsi+r_obsi - snd bp.cgeode,n_geode+r_geode), (r_ore,r_clay,r_obsi,r_geode+1)
    in
    let (n_ore, n_clay, n_obsi, n_geode), (r_ore, r_clay, r_obsi, r_geode) = inv, base in

    (* We start with all 5 choices / possibles "coups", and then filter them (put the corresponding index of the array at false : 0 for Economiser, 1 for Construire Ore ...) *)
    let coups = [|true;true;true;true;true|] in
    try
      (* uncraftable / too much *)
      if not (can_craft inv bp Ore) || r_ore >= max_ore then
        coups.(1) <- false;
      if not (can_craft inv bp Clay) || r_clay >= max_clay then
        coups.(2) <- false;
      if not (can_craft inv bp Obsi)  || r_obsi >= max_obsi then
        coups.(3) <- false;
      if not (can_craft inv bp Geode) then
        coups.(4) <- false;

      (* we made geode as we can (pretty greedy)*)
      if can_craft inv bp Geode then
        begin
          for i = 0 to 3 do coups.(0) <- false done;
          raise (Trouve coups);
        end
      else
        raise (Trouve coups)

     (* filter *)
    with Trouve coups -> (
        [(Economiser,inv,base) ; (Construire Ore,inv,base) ; (Construire Clay,inv,base) ; (Construire Obsi,inv,base) ; (Construire Geode,inv,base)]
        |> List.filteri (fun i _ -> coups.(i))
      )
  in
  (* ============================================== *)

  let beam = ref [(Economiser, (0,0,0,0), (1,0,0,0))] in
  for minutes = 1 to minutes_max do
    (* we build then edit the list of following possibilities *)
    let suivants = concat_rec_tale (map_rec_tale (fun (move,inv,base) -> max_n_plus_un inv base minutes move) !beam) in
    let suiv_sorted = List.sort comparePath suivants |> List.rev in

    let k_firsts = get_k_firsts suiv_sorted k in
    beam := k_firsts;
  done;
  List.fold_left (fun b (_,(_,_,_,ngeode),_) -> max ngeode b) 0 !beam

(* ====================================================================================================================================== *)

let compute_output (l : (int*blueprint) list) : int =
  (* part 1 *)
  let minutes_max = 24 in
  l
  |> map_rec_tale (fun (id,bp) -> id * (max_geode bp minutes_max k1))
  |> List.fold_left (+) 0

let compute_output2 l : int =
  (* part 2 *)
  let minutes_max = 32 in
  let three_firsts = get_k_firsts (List.rev l) 2 in
  three_firsts
  |> map_rec_tale (fun (id,bp) -> let m = max_geode bp minutes_max k2 in (* Printf.printf "id : %i | max : %i\n" id m ; *) m)
  |> List.fold_left ( * ) 1

let _ =
  Printf.printf "Partie 1 : %i\n" (compute_output data);
  Printf.printf "Partie 2 : %i\n" (compute_output2 data);
