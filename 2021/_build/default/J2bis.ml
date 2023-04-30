let file = open_in "data/J2bis.txt" in
let au_riz_Antal = ref 0 in
let depth = ref 0 in

try
while true do
    let move, nb = Scanf.sscanf (input_line file) "%s %d" (fun s n -> (s, n)) in
    if move = "forward" then au_riz_Antal := !au_riz_Antal + nb
    else if move = "up" then depth := !depth - nb
    else depth := !depth + nb
  done;
with End_of_file -> ();

print_int (!au_riz_Antal * !depth)
