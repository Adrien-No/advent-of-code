 (* lsdir cnbd85621 cqzvwldir dbnsfp114355 hbhp.cfvdir mcgqdir pcccpdir qpbqqj224038 rrcsndz.tzpdir zcsm27570 zjbvwsnv.fjt*)

let print_string_list l = Printf.printf "[\n";
  List.iter (fun s -> Printf.printf "%s\n" s) l; Printf.printf "]\n"

let _ =
  let ch = "dir cnbd85621 cqzvwldir dbnsfp114355 hbhp.cfvdir mcgqdir pcccpdir qpbqqj224038 rrcsndz.tzpdir zcsm27570 zjbvwsnv.fjt" in
  let r1 = Str.regexp "dir [a-z]+" in
  let r2 = Str.regexp "" in
  let r3 = Str.regexp "[0-9]+ [a-z]+" in
  print_string_list (Str.split r1 ch);
  Printf.printf "\n\n\n";
  print_string_list (Str.split r3 (Str.global_replace r2 "" ch))
