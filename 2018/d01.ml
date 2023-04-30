let rec lit_tout f =
  try
    let n = int_of_string (input_line f) in
    n::lit_tout f
  with End_of_file -> close_in f;[]

exception Trouve of int

let _ =
  let list = lit_tout (open_in "i1.txt") in
  let rec aux l acc =
    match l with
      [] -> acc
    | t::q -> aux q (acc+t)
  in
  Printf.printf "%i\n" (aux list 0);

(* partie 2 *)

  let t = Hashtbl.create 130000 in

  let rec aux l acc =
    match l with
      [] -> aux list acc
    | head::tail ->
      try
        let new_v = acc+head in
        if Hashtbl.mem t new_v then begin  Printf.printf "trouvé !\n"; raise (Trouve new_v) end
        else begin (*Printf.printf "On ajoute %i à la table\n" new_v;*)
          Hashtbl.add t new_v "teuchi";
          aux tail (new_v)
        end
      with Trouve v -> v
  in
  Printf.printf "%i" (aux list 0)
