let data =
  let f = Scanf.Scanning.from_channel open_in "file.txt" in
  let rec read l =
    try
      read (Scanf.bscanf "Valeur %i : %i" (fun x,y -> (x,y)::l))
    with End_of_file -> l
