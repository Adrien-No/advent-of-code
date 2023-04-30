Printexc.record_backtrace true

let file = open_in "/home/adriroot/Nextcloud/info/prog/ocaml/aoc21/data/J6.txt"
(*let print_int_list l = List.iter (fun x -> Printf.printf "%i," x) l ; Printf.printf "\n"*)

let fishs =
  let s = input_line file in
  String.fold_left (fun b x -> if x = ',' then b else (int_of_string (Char.escaped x)) ::b) [] s

let evolution n fishs =
  let days = Array.make 9 0 in
  (* on remplit de poissons *)
  List.iter (fun f -> days.(f) <- days.(f) + 1) fishs;

  let rec aux n =
    if n = 0 then
      days
    else
      let c = days.(0) in
      for i = 0 to 7 do
        days.(i) <- days.(i+1)
      done;
      days.(8) <- c;
      days.(6) <- days.(6) + c;
      aux (n-1)
  in
  aux n

let _ =
  (*print_int_list fishs; Printf.printf "\n\n\n";*)
  Printf.printf "%i" (Array.fold_left (+) 0 (evolution 256 fishs))

(* 300 : too low *)
