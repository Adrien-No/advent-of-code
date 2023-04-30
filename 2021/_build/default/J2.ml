let file = open_in "data/J2.txt"

let rec lit_fichier the_file =
    try
        let v = Scanf.sscanf (input_line the_file) "%s %d" (fun s n -> (s, n)) in
        v :: lit_fichier the_file
    with End_of_file -> [];; (* fichier fini on sort *)

let l = lit_fichier file in
let array = Array.of_list l in
let h = ref 0 in
let depth = ref 0 in
for i =  0 to (Array.length(array) -1) do
  if (fst array.(i)) = "forward" then
    h := !h + snd array.(i)
  else
  if (fst array.(i)) = "up" then
    depth := !depth - (snd array.(i))
  else
    depth := !depth + (snd array.(i))
done;
let h2 = ref 0 in
let depth2 = ref 0 in
let aim = ref 0 in
for i = 0 to (Array.length(array) -1) do
  if (fst array.(i)) = "forward" then
    begin
      h2 := !h2 + snd array.(i);
      depth2 := !depth2 + !aim * snd array.(i);
    end
  else
  if (fst array.(i)) = "up" then
      aim := !aim - snd array.(i)
  else
      aim := !aim + snd array.(i);
done;

print_int (!h * !depth);
print_newline();
print_int (!h2 * !depth2)
