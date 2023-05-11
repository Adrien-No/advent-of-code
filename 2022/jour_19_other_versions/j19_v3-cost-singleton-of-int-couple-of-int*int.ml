(* TODO : *)
(*   - finir la commande `pay` dans update_nb, *)
(*   - intégrer completement la nouvelle blueprint (certains coups sont des couples) *)
(*   - finir les entrées, le programme en entier *)
(*   - tester chacunes des fonctions *)
(*   - tester le tout sur des petites entrées (comparer avec autre algos pas efficace mais qui renvoie des solutions optimales) *)
(*   - eventuellement ajouter des filtres *)
(*   - finir. *)

type resource = Ore | Clay | Obsi | Geode
type choice = Construire of resource | Economiser
type cost = Singleton of int | Couple of int*int (* 2nd case for blueprint when two components are needed *)
type blueprint = resource -> cost

type state =
  {
    nb: resource->int; (* number of this resource *)
    rbt: resource->int; (* robots of this resource *)
  }

let minutes_max = 24

let change_vals (a,b,c,d) f (*: resource -> int*) = function
  (* returns a function that attribute a value for each constructor of resource *)
  | Ore -> f a
  | Clay -> f b
  | Obsi -> f c
  | Geode -> f d

let max_geode (bp : blueprint) =
  let s =
    (* state of the board at one moment *)
    {
      nb = change_vals (0, 0, 0, 0) Fun.id;
      rbt = change_vals (1, 0, 0, 0) Fun.id;
    }
  in
  let best_in_category = Array.make minutes_max ({nb = change_vals (0, 0, 0, 0) Fun.id ; rbt = change_vals (0, 0, 0, 0) Fun.id}) in
   let craftable s = function
     | Singleton c ->


    s.nb r >= bp r
  in
  let update_nb (s:state) (action:choice) : resource -> int =
    (* returns the new materials function of inventory *)
    let harvest s r : int =
      (* get resources from robots *)
      s.rbt r + s.nb r
    in
    let pay = function
      (* renvoie pour chaque action possible une fonction qui à une resource associe son coup après avoir payé pour l'action *)
      | Economiser -> s.nb
      | Construire r ->

        match bp r with
        | Singleton cout ->
          begin
            match r with
              Ore | Clay -> begin function Ore -> s.nb Ore - cout
                                         | _ -> s.nb r end
            | _ -> s.nb
          end
        | Couple (c1,c2) ->
          begin
            match r with
              Obsi | Geode -> begin
                function Ore -> s.nb Ore - c1
                       | Clay when r = Obsi -> s.nb Clay - c2
                       | Obsi when r = Geode -> s.nb Obsi - c2
                       | _ -> s.nb r
              end
            | _ -> s.nb
          end

    in
    change_vals (Ore, Clay, Obsi, Geode) (harvest s)
  in
  let update_rbt (s:state) (action:choice) : resource -> int =
    (* returns the new robots function, depending of the action *)
    match action with
    | Economiser -> s.rbt
    | Construire r ->
      let add_rbt r1 r2 =
        if r1 = r2 then (s.rbt r2)+1
        else s.rbt r2
      in
      change_vals (Ore, Clay, Obsi, Geode) (add_rbt r)
  in
  let is_best_in_smth s mins =
    [Ore ; Clay ; Obsi ; Geode]
    |> List.map (fun m -> s.nb m >= best_in_category.(mins).nb m)
    |> List.fold_left (||) false
    || [Ore ; Clay ; Obsi ; Geode]
       |> List.map (fun m -> s.rbt m >= best_in_category.(mins).rbt m)
       |> List.fold_left (||) false
  in

  let rec aux_max (s:state) (mins:int) (move:choice) =
    let s = {nb = update_nb s ; rbt = update_rbt s move} in

    let filter_need_ore l : choice list =
      (* it's the first one so we can assumes that l is full but when can't do this after *)
      if s.rbt Ore < bp Ore then
        begin
          if bp Ore >= bp Clay then
            [Construire Ore]
          else
            [Construire Ore;Construire Clay]
        end
      else
        l
    in
    let filter_can_do_smth l : choice list =
      if craftable s Ore && craftable s Clay && craftable s Obsi then
        List.filter ((<>)Economiser) l
      else l
    in
    let filter_geode_as_can l : choice list =
      if craftable s Geode then
        List.filter ((=)(Construire Geode)) l
      else
        l
    in
    (* ================ stop conditions ================ *)
    if mins = 24 then
      s.nb Geode
    else if is_best_in_smth s mins |> not then
      0
    (* ================================================= *)
    else
      [Economiser ; Construire Ore ; Construire Clay ; Construire Obsi ; Construire Geode]
       |> filter_need_ore
       |> filter_can_do_smth
       |> filter_geode_as_can

       |> List.filter (function Economiser -> true | Construire r -> craftable s r)
       |> List.map (aux_max s (mins+1))
       |> List.fold_left max 0
  in
  aux_max s 1 Economiser

let compute_output (l : (int*(resource->int)) list) : int =
  l
  |> List.map (fun (id,bp) -> let m = max_geode bp in Printf.printf "id : %i | max : %i\n" id m ; id * m)
  |> List.fold_left (+) 0

(* ================================================================ entrée ================================================================ *)
let data =
  let f = Scanf.Scanning.from_channel (open_in "j19.txt") in
  let rec read l =
    try
      read (Scanf.bscanf f "Blueprint %i: Each ore robot costs %i ore. Each clay robot costs %i ore. Each obsidian robot costs %i ore and %i clay. Each geode robot costs %i ore and %i obsidian.\n"
              (fun id ore_cost clay_cost obsi_cost_0 obsi_cost_1 geode_cost_0 geode_cost_1 ->
                 (id,{nb =  change_vals ()})) :: l)
    with End_of_file -> l
  in
  read []
(* ======================================================================================================================================== *)

        (* match r with *)
        (*   Ore -> (let Singleton cout = bp r in *)
        (*       function Ore -> s.nb Ore - cout | r -> s.nb r) *)
        (* | Clay -> (let Singleton cout = bp r in *)
        (*       function Ore -> s.nb Ore - cout | r -> s.nb r) *)
        (* | Obsi -> (let Couple (c1,c2) = bp r in *)
        (*       function Ore -> s.nb Ore - c1 *)
        (*              | Clay -> s.nb Clay - c2 *)
        (*              | r -> s.nb r *)
        (*           ) *)
        (* | Geode -> (let Couple (c1,c2) = bp r in *)
        (*            function Ore -> s.nb Ore - c1 *)
        (*                   | Obsi -> s.nb Ore - c2) *)



        (* match bp r with *)
        (* | Singleton c -> *)
        (*   match c with *)
        (*     Ore -> s.nb r - c *)
        (*   | Clay -> *)
        (* | Couple(c1,c2) -> if r = Ore then s.nb r - c1 *)
        (*                                    else s.nb r - c2 *)
