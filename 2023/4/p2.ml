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

let cards = Array.make (List.length entry2) 1
let matching_numbs = Array.make (List.length entry2) 0

let incr_k_next_n_times i k n =
  for _ = 0 to n-1 do
    for j = 1 to k do
      cards.(i+j) <- cards.(i+j) + 1
    done
  done

let _ =
  entry2
  |> List.map (fun (_, l1, l2) -> List.filter (fun x -> List.exists ((=)x) l2) l1)
  |> List.iteri (fun i l -> matching_numbs.(i) <- (List.length l));

  for i = 0 to (Array.length cards) -1 do
    incr_k_next_n_times i matching_numbs.(i) cards.(i)
  done;

  Array.fold_left (+) 0 cards
  |> Printf.printf "%i\n"
