let file = open_in "data/J1.txt"
let rec lit_fichier the_file =
    try
        let v = Scanf.sscanf (input_line the_file) "%d" (fun n -> n) in
        v :: lit_fichier the_file
    with End_of_file -> [];; (* fichier fini on sort *)

let l = lit_fichier file in
let array = Array.of_list l in
let past_value = ref array.(0) in
let counter1 = ref 0 in

(* Part 1 *)
for i = 0 to Array.length (array) -1 do
  let new_value = array.(i) in
  if !past_value < new_value then
    counter1 := !counter1 + 1;
  past_value := new_value;
done;

(* Part 2*)
let old_sum = ref(array.(0) + array.(1) + array.(2)) in
let counter2 = ref 0 in
for i = 3 to Array.length (array) -1 do
  let new_sum = array.(i-2) + array.(i-1) + array.(i) in
  if !old_sum < new_sum then
    counter2 := !counter2 +1;
  old_sum := new_sum;
done;
print_int !counter1;

print_newline();
print_int !counter2;

print_newline();
print_newline();
print_int (Array.length array);
