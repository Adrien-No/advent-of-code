let rec lit_tout f =
  try
    let n = input_line f in
    n :: lit_tout f
  with End_of_file -> close_in f; []

let is_exact (l:string) (n:int) : bool =
  let count = Array.make 26 0 in
  l |> String.iter (fun c -> let i = Char.code c - 97 in count.(i) <- count.(i)+1);
  Array.mem n count

let count_exact l n =
  l |> List.fold_left (fun b x ->
      if is_exact x n
      then b + 1
      else b) 0

exception Wrong
let are_correct (s1:string) (s2:string) =
  let c = ref 0 in
  try
    for i = 0 to 25 do
      if s1.[i] <> s2.[i] then begin incr c; if !c > 1 then raise Wrong end
    done;
    if !c = 0 then false else
    true
  with Wrong -> false

exception Trouve of (string*string)

let common s1 s2 =
  let com = ref [] in
  for i = 0 to String.length s1 -1 do
    if s1.[i] = s2.[i] then com := s1.[i] :: !com
  done;
  !com |> List.rev

let rec print_char_list l =
  match l with
    [] -> Printf.printf "\n"
  | t::q -> Printf.printf "%c" t; print_char_list q

let parcours (l:string list) : string*string =
  let t = Array.of_list l in
  let n = Array.length t in
  try
    for i = 0 to n-1 do
      for j = i to n-1 do
        if are_correct t.(i) t.(j) then raise (Trouve (t.(i), t.(j)) );
      done;
    done;
    failwith "pas en commun"
  with Trouve v -> v

let _ =
  let l = "i2.txt" |> open_in |> lit_tout in
  Printf.printf "Part 1 : %i\n" ((count_exact l 2) * (count_exact l 3));

  let s1, s2 = parcours l in
  Printf.printf "Part 2 : "; print_char_list (common s1 s2)
(* not 22
 * x < 5500 *)
