Printexc.record_backtrace true

(*let file = open_in "j5.txt"*)
let f = Scanf.Scanning.open_in "j5.txt" (*file*)
let piles = Array.map (fun l -> List.rev l)[|
  ['C';'Z';'N';'B';'M';'W';'Q';'V'];
  ['H';'Z';'R';'W';'C';'B'];
  ['F';'Q';'R';'J'];
  ['Z';'S';'W';'H';'F';'N';'M';'T'];
  ['G';'F';'W';'L';'N';'Q';'P'];
  ['L';'P';'W'];
  ['V';'B';'D';'R';'G';'C';'Q';'J'];
  ['Z';'Q';'N';'B';'W'];
  ['H';'L';'F';'C';'G';'T';'J']
|]

(*let piles =
  [|
    ['N';'Z'];
    ['D';'C';'M'];
    ['P'];
  |]*)

let print_char_list l =
  List.iter (fun c -> Printf.printf "%c" c) l

let moves =
  let rec aux l =
    try
      aux (Scanf.bscanf f "move %i from %i to %i\n" (fun a b c -> (a,b,c)) :: l)

    with End_of_file -> List.rev l
  in
  aux []

let move src dst =
  match src with
    [] -> failwith "source vide"
  | t::q -> q,t::dst

let arrangment (l:char list array) (m:(int*int*int) list) : char list array =
  let rec aux m  =
    match m with
      [] -> l
    | (nb,i_src,i_dst)::q ->
      (*Printf.printf "move %i from %i to %i\n" nb i_src i_dst;*)
      (*      if (nb = 3 && i_src = 1-1 && i_dst = 3-1) || (nb = 2 && i_src = 2-1 && i_dst = 1-1) then begin
        Printf.printf "\nsrc "; print_char_list l.(i_src-1);
              Printf.printf " |dst "; print_char_list l.(i_dst-1);*)
        for _ = 0 to nb-1 do (*if l.(i_src) <> [] && l.(i_dst) <> [] then begin*)
          let src, dst = move l.(i_src-1) l.(i_dst-1) in
          l.(i_src-1) <- src;
          l.(i_dst-1) <- dst;
        done;
        let rec head_rev n h dst =
          if n = 0 then (h,dst)
          else match dst with
              [] -> failwith "peupa"
            | t::q -> head_rev (n-1) (t::h) q
        in
        let h_l,q_l = head_rev nb [] l.(i_dst-1) in l.(i_dst-1) <- List.concat[h_l;q_l];
      (*  Printf.printf "\nsrc "; print_char_list l.(i_src-1);
        Printf.printf " |dst "; print_char_list l.(i_dst-1); Printf.printf "\n"
      end
      else begin for _ = 0 to nb-1 do (*if l.(i_src) <> [] && l.(i_dst) <> [] then begin*)
          let src, dst = move l.(i_src-1) l.(i_dst-1) in
          l.(i_src-1) <- src;
          l.(i_dst-1) <- dst;
        done;
        end;*)
      aux q
  in
  aux m

let not_empty t = Array.fold_left (fun b x -> b || (x <> [])) false t

let _ =
  let arr = arrangment piles moves in
  Array.iter (fun l ->
      match l with
        [] -> ()
      | t::_ -> Printf.printf "%c" t) arr;

  Printf.printf "\nliste par liste :\n";
  Array.iter print_char_list arr;

  Printf.printf "\nde haut en bas\n";
  while not_empty arr do
    Array.iteri (fun i l ->
        match l with
          [] -> ()
        | t::q -> Printf.printf "%c" t; arr.(i) <- q) arr
  done

(* part 1:
 *  RZBGZN *)

(* part2 : QZHFJTNZW
 * QNHWJVJZW *)
