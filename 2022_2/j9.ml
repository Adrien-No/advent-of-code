Printexc.record_backtrace true
let f = Scanf.Scanning.from_file "j9.txt"

let print_char_int_list l = List.iter (fun (m,i) -> Printf.printf "(%c,%i) " m i) l; Printf.printf "\n"
let print_plateau t =
  let m = Array.length t in
  let n = Array.length t.(0) in
  for j = 0 to n-1 do
    for i = 0 to m-1 do
      Printf.printf "%c" t.(i).(j)
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n\n"

let moves =
  let rec lit l =
    try
      lit (Scanf.bscanf f "%c %i\n" (fun c i -> (c,i)) :: l)
    with End_of_file -> l
  in
  lit [] |> List.rev

let _ = print_char_int_list moves

let find_dims l =
  let rec aux (lst:(char*int) list) (mu:int) (u:int) (ml:int) (l:int) (mr:int) (r:int) (md:int) (d:int) =
    match lst with
      [] -> (mu,ml,mr,md)
    | (c,v)::q ->
      match c with
      'U' -> aux q (max mu (u+v)) (u+v) ml l mr r (max md (d-v)) (d-v)
      | 'L' -> aux q mu u (max ml (l+v)) (l+v) (max mr (r-v)) (r-v) md d
      | 'R' -> aux q mu u (max ml (l-v)) (l-v) (max mr (r+v)) (r+v) md d
      | 'D' -> aux q (max mu (u-v)) (u-v) ml l mr r (max md (md+v)) (md+v)
      | _ -> failwith "unrecognized"
  in
  aux l 0 0 0 0 0 0 0 0

let (dimensions:int*int),(depart:int*int) =
(*  let u,l,r,d = List.fold_left (fun (u,l,r,d) (m,v) ->
      match m with
      'U' -> (u+v, l, r , d)
      | 'L' -> (u, l+v, r, d)
      | 'R' -> (u, l, r+v, d)
      | 'D' -> (u, l, r, d+v)
      | _ -> failwith "unrecognized move")
      (0, 0, 0, 0) moves
    in*)
  let (u,l,r,d) = find_dims moves in
  Printf.printf "u=%i l=%i r=%i d=%i\n" u l r d;
  (l+r+1,u+d+1 ), (l, u)

let print_state xH yH xT yT =
  let t = Array.make_matrix (fst dimensions) (snd dimensions) '.' in
  t.(xH).(yH) <- 'H';
  t.(xT).(yT) <- 'T';
  print_plateau t

let follow xH yH xT yT last_m : (int*int) =
  (* renvoie la pos de T tq il suit H *)
  let dist_x = abs(xH-xT)
  and dist_y = abs(yH-yT) in
  if (xH = xT && yH = yT) || dist_x = dist_y && dist_x = 1 then xT,yT else (* cas où dist_x = dist_y = 0*)
  if yT < yH (*&& xT <> xH*) && dist_x < dist_y then
    (xH,yH-1)
  else if yT > yH && dist_x < dist_y then
    (xH, yH+1)
  else if xT < xH && dist_x > dist_y then
    (xH-1, yH)
  else if xT > xH && dist_x > dist_y then
    (xH+1,yH)
  else if xT = xH then
    match last_m with
    'R' -> (xH,yH-1)
    | 'L' -> (xH,yH+1)
    | _ -> failwith "cas pas matché 1"
  else if yT = yH then
    match last_m with
    'D' -> (xH+1,yH)
    | 'U' -> (xH-1, yH)
    | _ -> failwith "cas pas matché 2"
  else if dist_x = dist_y then
    match last_m with
    'U' -> (xH-1,yH)
    | 'L' -> (xH,yH-1)
    | 'R' -> (xH,yH+1)
    | 'D' -> (xH+1,yH)
    | _ -> failwith "cas pas matché 3"
  else failwith "cas pas trouvé fin"

let count plateau = Array.fold_left (fun b t -> b + Array.fold_left (+) 0 (Array.map (fun c -> if c = '#' then 1 else 0) t)) 0 plateau
let part1 plateau n m x y=
  let _ = Printf.printf "dim, depart : %i %i %i %i\n" n m x y in

  let rec do_the_moves l xH yH xT yT =
    (*print_plateau plateau;*)

    match l with
      [] -> ()
    | (m,v)::q ->
    let move_H (m:char) i : (int*int) =
      match m with
      'U' -> (xH,yH-i)
      | 'L' -> (xH-i,yH)
      | 'R' -> (xH+i,yH)
      | 'D' -> (xH,yH+i)
      | _ -> failwith "unrecognized move_H"
    in
    let temp_xH,temp_yH = ref xH, ref yH in
    let temp_xT, temp_yT = ref xT, ref yT in
    (*Printf.printf "===== %c %i =====\n" m v*)
    for i = 1 to v do
      let xH, yH = move_H m i in
      temp_xH := xH;
      temp_yH := yH;
      let xT,yT = follow !temp_xH !temp_yH !temp_xT !temp_yT m in
      temp_xT := xT;
      temp_yT := yT;
      (*Printf.printf "xH,yH calculés : %i %i\n" temp_xH temp_yH;
        Printf.printf "xT,yT calculés : %i %i\n" xT yT;*)
      plateau.(!temp_xT).(!temp_yT) <- '#'
      (*print_state !temp_xH !temp_yH !temp_xT !temp_yT*)
    done;
    do_the_moves q !temp_xH !temp_yH !temp_xT !temp_yT
  in
  do_the_moves moves x y x y

let _ =
    let (n, m),(x,y) = dimensions, depart in
  let plateau = Array.make_matrix n m '.' in
  part1 plateau n m x y;
  print_plateau plateau;
  Printf.printf "Solution partie 1 :%i" (count plateau)
