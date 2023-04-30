let data =
    let f = open_in "j3_max.txt" in
    let fc = Scanf.Scanning.from_channel f in
    let rec lit () =
        try
            let x =
                Scanf.bscanf fc "%s\n"
                (fun x -> (x))
            in
            (x) :: lit ()
        with End_of_file -> []
    in Array.of_list (lit ())

exception Find of char
let get_same (a:string) : char =
  (* renvoie le caractère en double dans la chaine a *)
  let n = (String.length a) / 2 in
  let h = Hashtbl.create n in
  (*Printf.printf "%s\n" a;*)
  try
    (*Printf.printf "m1 = %i | n = %i\n" n (String.length a);*)
  for i = 0 to n-1 do
    Hashtbl.add h a.[i] true
  done;

  for i = n to String.length a do
    (*Printf.printf "%c" a.[i];*)
    if Hashtbl.find_opt h a.[i] <> None then raise (Find a.[i])
  done;
  '+'
  with Find x -> x

let same_bet_2 s1 s2 : char list =
  let n1 = String.length s1
  and n2 = String.length s2 in
  let h = Hashtbl.create (n1+n2) in

  String.iter (fun c -> Hashtbl.add h c true) s1;
  String.fold_left (fun b x -> if Hashtbl.find_opt h x <> None then x:: b else b) [] s2


let same_2_3 (l:char list) (s3:string) =
  try
  let h = Hashtbl.create (List.length l) in
  List.iter (fun x -> Hashtbl.add h x true) l;
  String.iter (fun x -> if Hashtbl.find_opt h x <> None then raise (Find x)) s3;
  '+'
  with Find x -> x

let _ =
  let d = data in
  Printf.printf "%i\n" (Array.fold_left (fun b x -> b + let c = get_same x in (*Printf.printf "%c\n" c;*)
                                                  if c = Char.lowercase_ascii c
                                                  then (Char.code c) - 96
                                                  else  (Char.code c) - 64 + 26) 0 d);
  let i = ref 0 in
  let s = ref 0 in
  while !i < Array.length data -2 do
    let s1, s2, s3 = d.(!i), d.(!i+1), d.(!i+2) in
    let c = same_2_3 (same_bet_2 s1 s2) s3 in
    s := !s + if c = Char.lowercase_ascii c
    then begin Printf.printf "%c | %i\n" c ((Char.code c) - 96); (Char.code c) - 96 end
    else begin Printf.printf "%c | %i\n" c ((Char.code c) - 96 + 26); (Char.code c) - 64 + 26 end;
i := !i+3;
  done;
  Printf.printf "%i" !s

(* avec "!i < Array.length data -3" 81762673 *)
(* avec "-2": 81762689 *)
