open List

let f = "entry.txt" |> open_in |> Scanf.Scanning.from_channel

let seeds' = Scanf.bscanf f "seeds: %[0-9 ]\n" Fun.id |> String.split_on_char ' ' |> List.map int_of_string

let seeds'' =
  let rec aux acc = function
    | [] -> rev acc
    | h1::h2::t -> aux ((h1, h2)::acc) t
    | _ -> failwith "error"
  in
  aux [] seeds'

let seeds = List.fold_left (fun acc (src, range) -> Printf.printf "seeds : %i\n" (length acc); List.sort_uniq Stdlib.compare (List.concat[(List.init range (fun i -> i+src));acc]) ) [] seeds''

let maps = (* ! lit pas la dernière ligne, rajouter "\ntruc map:" à la fin du fichier *)
  let rec read_maps acc =
    try
      ignore(Scanf.bscanf f "\n%s map:\n" Fun.id);
      let rec read_3list acc =
        try
          let l' = Scanf.bscanf f "%i %i %i\n" (fun x y z -> (x, y, z)) in
          read_3list (l'::acc)
        with Stdlib.Scanf.Scan_failure s ->  rev acc
      in
      read_maps (read_3list [] :: acc)

    with End_of_file -> rev acc
  in
  read_maps []

let build_function l =

  let rec improve_f f = function
    | [] -> f
    | (dst, src, range)::t -> improve_f
              (function | x when src <= x && x < src+range -> (x-src) + dst
                        | x -> f x
              ) t
  in
  improve_f Fun.id l

let build_functions (m: (int*int*int) list list) =
  List.map build_function m

let rec apply_fcts fcts x = (* Printf.printf "%i " x; *) match fcts with
  | [] -> x
  | h::t -> apply_fcts t (h x)

let find_lower seeds =
  let fcts = build_functions maps in
  (* Printf.printf "len seeds: %i" (List.length seeds); *)
  seeds
  |> List.map ((* Printf.printf "\n"; *) apply_fcts fcts)
  |> List.fold_left (min) max_int

(* let _ = *)
(*   Printf.printf "seeds : %i\n" (length seeds) *)
  (* Printf.printf "\n%i\n" (find_lower seeds) *)
