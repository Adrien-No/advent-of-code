open List

let entry2 =
  let f = "entry.txt" |> open_in |> Scanf.Scanning.from_channel in
  let rec read acc =
    try
      let l = Scanf.bscanf f "Card %i: %[ 0-9] | %[0-9 ]\n" (fun i s s' ->
          (i,
           s  |> String.split_on_char ' ' |> filter ((<>)"") |> List.map int_of_string ,
           s' |> String.split_on_char ' ' |> filter ((<>)"") |> List.map int_of_string)
        ) in read (l::acc)
    with End_of_file -> rev acc
  in read []

let rec pow2 = function
  | 0 -> 0
  | 1 -> 1
  | n -> 2 * pow2 (n-1)

let _ =
  entry2
  |> List.map (fun (_, l1, l2) -> List.filter (fun x -> List.exists ((=)x) l2) l1)
  |> List.map (fun l -> pow2 (List.length l))
  |> List.fold_left (+) 0
  |> Printf.printf "%i\n"
