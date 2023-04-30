(*let file = open_in "j2.txt"

let data =
  let f = Scanf.Scanning.from_channel file in
  let rec read l =
    try
      read (Scanf.bscanf f "%c %c" (fun x y -> (x,y)) :: l)
    with End_of_file -> []
  in
  read []

let _ =
  let data = data in
*)
