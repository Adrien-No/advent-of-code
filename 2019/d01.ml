Printexc.record_backtrace true
let rec lit_tout f =
  try
    let n = (int_of_string (input_line f)) in
            n:: lit_tout f
  with End_of_file -> close_in f; []

let fuel_needed (poid:int) : int =
  int_of_float ((float_of_int poid) /. 3. ) -2

let _ =
  let t = lit_tout (open_in "i1.txt") |> Array.of_list in
  let n = Array.length t in
  let fuel = Array.init n (fun i -> fuel_needed t.(i)) in
  let sum = Array.copy fuel in

  for _ = 0 to 1000 do
    Printf.printf "n = %i; len(sum) = %i\n" n (Array.length sum);
    Array.iteri (fun i _-> let f = fuel_needed fuel.(i) in
                 if f > 0
                 then begin
                   fuel.(i) <- f;
                   sum.(i) <- sum.(i) + f;
                 end) fuel;
  done;
  Array.fold_left (+) 0 sum |> Printf.printf "%i"


  (*let sum = ref (!fuel) in
  while fuel_needed !fuel > 0 do
    sum := !sum +  (fuel_needed !fuel);
    fuel := fuel_needed !fuel;
  done;
    Printf.printf "%i" !sum*)
  (* let fuel4fuel = ref (int_of_float ((float_of_int !sum) /. 3.) -2) in
   * while !fuel4fuel > 0 do
   *   let new_fuel = ref (int_of_float ((float_of_int !fuel4fuel) /. 3.) -2) in
   *   sum := !sum + !new_fuel;
   *   fuel4fuel := !new_fuel
   * done;
   * Printf.printf "%i" !sum *)


(* 4027578 < x < 5178325 *)
