let file = open_in "data/J4.txt" in
for i=0 to 2 do
  let current = Scanf.sscanf (input_line file) "%d," (fun a -> a) in
  print_int current;
  print_int i;
done;
