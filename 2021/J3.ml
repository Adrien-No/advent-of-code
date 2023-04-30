let file = open_in "data/J3.txt" in
let len_word = 12 in
let nb_words = 1000 in
let gamma = Array.make len_word 2 in
let epsilon = Array.make len_word 2 in
let s_col = Array.make len_word 0 in

try
  while true do
    let a = Scanf.sscanf (input_line file) "%s" (fun a -> a) in
    for i=0 to len_word-1 do
      print_int (int_of_char a.[i] - 48);
      print_string " ";
      s_col.(i) <- s_col.(i) + int_of_char a.[i] - 48;
    done;
  done;
with End_of_file -> ();

  (* PART I *)
  for i = 0 to len_word-1 do
    print_int s_col.(len_word-1-i);
    print_newline();
    if s_col.(len_word-1-i) > nb_words/2 then
    begin
    gamma.(len_word-1-i) <- 1;
    epsilon.(len_word-1-i) <- 0;
  end
  else
    begin
    gamma.(len_word-1-i) <- 0;
    epsilon.(len_word-1-i) <- 1;
    end
done;
(*print_newline();
affichage
   for i = 0 to len_word-1 do
    print_int gamma.(i)
  done;

  print_newline();

  for i = 0 to len_word-1 do
        print_int epsilon.(i)
     done;*)
  (* *)
