open List

let entry2 =
  let f = "testtest.txt" |> open_in |> Scanf.Scanning.from_channel in
  let rec read acc =
    try
      let l = Scanf.bscanf f "%[0-9 ] | %[0-9 ]\n" (fun s s' -> (s, s')) in read (l::acc)
    with End_of_file -> rev acc
  in read []
