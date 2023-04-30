(*let file = open_in "data/J4.txt" in
while not (End_of_file) do
  let current = Scanf.sscanf (input_line file) "%d," (fun a -> a) in
  print_int current;
  print_int i;
done;
*)
