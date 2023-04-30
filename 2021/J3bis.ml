(*let file = open_in "data/J3.txt" in
let len_word = 12 in
let nb_words = ref 1000 in

(* POTENTIELS *)
let potentiels_ox = Array.make !nb_words 0 in
let potentiels_co2 = Array.make !nb_words 0 in
try
  for i=0 to !nb_words-1 do
    let a = Scanf.sscanf (input_line file) "%d" (fun a -> a) in
    potentiels_ox.(i) <- a;
    potentiels_co2.(i) <- a;
  done;
with End_of_file -> ();
(*          *)

  (* OXYGEN *)
let index = ref 0 in
let next_nb_words = nb_words in
while !next_nb_words > 1 do

  (* bit criteria && next_nb_words*)
  let criteria = ref 0 in
  for i = 0 to !next_nb_words-1 do
    criteria := !criteria + int_of_char (string_of_int potentiels_ox.(i)).[!index]
  done;
  if (!next_nb_words/2) >= !criteria then
    begin
    nb_words := !next_nb_words;
    next_nb_words := !criteria;
    criteria := 1 end
  else
    begin
    nb_words := !next_nb_words;
    next_nb_words := !criteria;
    criteria := 0 end;
  (**)

  (* new_potentiels *)
  let new_potentiels = Array.make next_nb_words 0 in
  for i = 0 to nb_words-1 do


done;
*)
