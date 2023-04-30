let data =
  let file = open_in "j4.txt" in
  let f = Scanf.Scanning.from_channel file in
  let rec aux l =
    try
      let a1,a2,b1,b2 = Scanf.bscanf f "%i-%i,%i-%i\n" (fun a1 a2 b1 b2 -> (a1,a2,b1,b2)) in
      aux (((a1,a2),(b1,b2)) ::l)
    with End_of_file -> l
  in
  aux []

let _ =
  Printf.printf "%i"
    (List.fold_left
       (fun b ((a1,a2),(b1,b2)) -> (*Printf.printf "%i-%i,%i-%i\n" a1 a2 b1 b2;*)
          if (a1 <= b1 && b1 <= a2 || b1 <= a1 && a1 <= b2)
          (*if (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2)*)
         then b+1
         else b)
       0 data)

(*940*)
