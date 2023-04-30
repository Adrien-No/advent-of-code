Printexc.record_backtrace true

type tree = {
  parent : tree option;
  mutable size : int;
  nom : string;
  enfants : (string,tree) Hashtbl.t;
  mutable fichiers : (string*int) list; (* (nom, taille) *)
  }

let f = Scanf.Scanning.from_file "j7.txt"

let print_tree (t:tree) =
  (* spc permet de compter le la profondeur actuelle de l'arbre *)
  Printf.printf "\n\nOn affiche l'arbre :\n";
  let rec aux (t:tree) (spc:int) : unit =
    for _ = 0 to spc-1 do Printf.printf " " done;
    Printf.printf "- %s (dir, size = %i)\n" t.nom t.size;
    List.iter (fun (n,t) -> for _ = 0 to spc-1 do Printf.printf " " done; Printf.printf "  - %s (file, size=%i)\n" n t) t.fichiers;
    Hashtbl.iter (fun _ arbre -> aux arbre (spc+1)) t.enfants
  in
  aux t 0

let print_string_list l = Printf.printf "[\n";
  List.iter (fun s -> Printf.printf "%s\n" s) l; Printf.printf "]\n"

let string_rev s =
  String.fold_left  (fun b x -> (Char.escaped x)^b) "" s

let instructions =
  let rec lit s =
  try
    let c = Scanf.bscanf f "%c" Fun.id in
    (*if c <> '\n' then*) lit (s^(Char.escaped c)) (*else lit s*)
  with End_of_file -> s
  in
  let s = lit "" in
  (*Printf.printf "%s" s;*)
  let instrs = String.split_on_char '$' s in
  List.iter (fun s -> Printf.printf "%s\n" s) instrs;
  instrs
(*List.map (fun s ->) instrs*)

let remove_nth_str (s:string) (n:int) : string =
  let lens = String.length s in
  if lens < n then failwith "grand ecart";
  String.init (lens-n) (fun i -> s.[i+n])

let remove_nth_last s n =
  String.init (String.length s -n) (fun i -> s.[i])

let split_on_slashn ch : 'a list =
  let n = String.length ch in
  let l = List.init n (fun i -> ch.[i]) in

  let rec aux l grande petite =
    match l with
      [] -> grande
    | t::q when t = '\\' -> begin
      match q with
        [] -> aux q grande (t::petite)
      | t2::q2 when t2 = 'n' -> aux q2 (petite::grande) []
      | _ -> aux q grande (t::petite)
    end
    | t::q -> aux q grande (t::petite)
  in
  let res = List.map List.rev (aux l [] []) in
  List.map (fun l ->
      let n = List.length l in
    let ch = String.init n (fun i -> List.nth l i) in ch) res (* ouais on en a plus rien a ficher *)

let del_empty (l: string list) = List.fold_left (fun b s -> if s = "" then b else s::b ) [] l

let build_tree t instrs =
  (* current tree *)
  let ct = ref t in
  Hashtbl.add !ct.enfants "/" {parent = Some !ct ; size = 0 ; nom = "/" ; enfants = Hashtbl.create 100 ; fichiers = []};
  List.iteri (fun i instr -> Printf.printf "instr_%i =%s\n" i instr; if instr <> "" then
      if instr.[1] = 'c' (* 1 car y'a un espace avant *)
      (*cd *)
      then
        (*cd .. *)
        if instr.[4] = '.' then
          ct := Option.get !ct.parent

        (*cd foo *)
        else begin
          let dir = remove_nth_str instr 4 in (* pas touche au 4 nn plus *)
          let dir = remove_nth_last dir 2 in
          Printf.printf "dir =%s\n" dir;
          ct := Hashtbl.find !ct.enfants dir
        end
     (* ls *)
      else
        (* liste des fichiers ou dossiers du ls *)
        let ch = remove_nth_str instr 3 in (* 3 pas deux jsp pq - si enft c a coz du decalage du a un espace avant chaque commande*)
        Printf.printf "file_dir avant split: %s\n" ch;
        let file_dir = split_on_slashn (ch) |> del_empty in
        Printf.printf "après :"; print_string_list file_dir;
        (*Printf.printf "caracs : %c %c %c %c\n" ch.[0] ch.[1] ch.[2] ch.[3];*)
        List.iter (fun str ->

            (* dir : "dir foo" *)
            if str.[0] = 'd' then
              let dir = remove_nth_str str 4 in
              (*if Hashtbl.mem !ct.enfants dir |> not then *)
                 (* on crée un nouveau dossier*)
              Hashtbl.add !ct.enfants dir {parent = Some !ct ; size = 0 ; nom = dir ; enfants = Hashtbl.create 100 ; fichiers = []};
              (*dans tous les cas on va dedans*)
                (*ct := Hashtbl.find !ct.enfants dir*)

            (* file : "taille foo" *)
            else
              let datas = String.split_on_char ' ' str |> Array.of_list in
              Printf.printf "datas :\n";print_string_list (Array.to_list datas);
              !ct.fichiers <- (datas.(1),int_of_string datas.(0))::!ct.fichiers
          ) file_dir

    ) instrs
(*
type tree = {
  parent : tree option;
  size : int;
  nom : string;
  enfants : (string,tree) Hashtbl.t;
  mutable fichiers : (string*int) list; (* (nom, taille) *)
}
  *)

let compte_size t : unit =
  (* pour mettre à jour la taille des dossiers, va etre appelé par un de ses enfants *)
  t.size <- List.fold_left (+) 0 (List.map (snd) t.fichiers) + (Hashtbl.fold (fun (_:string) (t2:tree) (b:int)-> b + t2.size) t.enfants 0)

let rec dfs_add_size t =
  Hashtbl.iter (fun _ e -> dfs_add_size e) t.enfants;
  compte_size t

let dfs_get_sum_size_above_n (t:tree) : int =
  let s = ref 0 in
  let rec dfs t =
    Hashtbl.iter (fun _ e -> dfs e) t.enfants;
    if t.size <= 100000 then s:= !s + t.size
  in
  dfs t;
  !s

let dfs_save_size t =
  let sizes = ref [] in
  let rec dfs t =
    Hashtbl.iter (fun _ e -> dfs e) t.enfants;
    sizes := t.size :: !sizes
  in
  dfs t;
  List.sort compare !sizes

let _ =
  let t = { parent = None ; size = 0; nom = ""; enfants = Hashtbl.create 100 ; fichiers = []} in
  (*let _ = instructions in*)
  build_tree t instructions;
  print_tree t;
  dfs_add_size t;
  print_tree t;
  let sol1 = (dfs_get_sum_size_above_n t) in
  Printf.printf "Réponse 1 : %i\n" sol1;

  let sizes = dfs_save_size t |> Array.of_list in

  let free = 70000000 - t.size in
  let i = ref 0 in
  while free + sizes.(!i) < 30000000 do
    incr i
  done;
  Printf.printf "Réponse 2 : %i" sizes.(!i)
