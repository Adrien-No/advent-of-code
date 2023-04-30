Printexc.record_backtrace true

let file = open_in "j1.txt"

(*let print_int_list_list l =
  Printf.printf "debut :\n";
  List.iter (fun l2 -> List.iter (fun x -> Printf.printf "%i\n" x) l2) l;
  Printf.printf "fin \n"*)

let print_list l : unit =
  Array.iter (fun x -> Printf.printf "%i\n "x) l

let rec read f grosse_liste petite_liste =
  try
    let line = input_line f in
    match line with
      "" -> read f (petite_liste :: grosse_liste) []
    | _ -> (*Printf.printf "string = \"%s\"\n" line;*)
      read f grosse_liste ((int_of_string line) :: petite_liste)

  with End_of_file -> grosse_liste

let _ =
  let t = read file [] [] in
  (*print_int_list_list t;*)
  let t = List.sort compare (List.map (fun x -> (List.fold_left (+) 0 x)) t) |> Array.of_list in
  Array.sort compare t;
  print_list t
