Printexc.record_backtrace true

let rec lit_tout f =
  try
    let n = int_of_string (input_line f) in
    n::lit_tout f
  with End_of_file -> close_in f;[]


(* let print_int_array t =
 *    for i = 0 to Array.length t -1 do
 *      Printf.printf "%i " t.(i)
 *    done;
 *    Printf.printf "\n\n\n" *)

exception Depassement

let try_couple t noun verb : (int*int) option =
  let t2 = Array.copy t in
  t2.(1) <- noun;
  t2.(2) <- verb;
  let i_case = ref 0 in
  try
  while t2.(!i_case) <> 99 do
    let pos1, pos2, pos3 = t2.(!i_case + 1), t2.(!i_case + 2), t2.(!i_case + 3) in
    if pos1 < Array.length t2 - 4 && pos2 < Array.length t2 - 4 &&  pos3 < Array.length t2 - 4 then
    match t2.(!i_case) with
      1 -> t2.(pos3) <- t2.(pos1) + t2.(pos2); i_case := !i_case + 4
    | 2 -> t2.(pos3) <- t2.(pos1) * t2.(pos2); i_case := !i_case + 4
    | _ -> Printf.printf "\n\n\npb ici : pos=%i, val=%i" !i_case t2.(!i_case);failwith "error, unknown opcode"
  else
    begin (*Printf.printf "dep %i : %i\n" noun verb;*) raise Depassement end
  done;

    if t2.(0) = 19690720
    then Some (noun, verb)
    else None
  with Depassement -> ();None

let _ =
  let t = Array.of_list (lit_tout (open_in "i2.txt")) in
  for i = 0 to 10000 do
    for j = 0 to 10000 do
      if try_couple t i j |> Option.is_some then Printf.printf "%i" (100*i + j)
    done;
  done;
  Printf.printf "testé !"
