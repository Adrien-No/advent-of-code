type elt = Int of int | Char of char

let get_elt = function Int i -> i | _ -> failwith "get_elt: error"

let entry =
  Array.init (read_int()) (fun _ ->
      read_line()
      |> String.fold_left (fun (acc, token) c ->
          if Char.code c >= 48 && Char.code c <= 57
          then (acc, Int (10*(get_elt token) + (c |> Char.escaped |> int_of_string)))
          else if token <> Int 0
          (* on ajoute token et c à acc *)
          then (Char c::token::acc, Int 0)
          else (Char c::acc, Int 0)
        ) ([], Int 0)
      |> (* pour gérer le cas où la ligne se fini par un entier *)
      (fun (acc, token) ->
         match token with
         | Int i when i <> 0 -> Int i :: acc
         | _ -> acc
      )
      |> List.rev
      |> Array.of_list
    )

let is_partn t (i, j) =
  let lenx = Array.length t
  and leny = Array.length t.(0) in

  [(i-1, j-1); (i-1, j); (i, j-1);
   (i, j); (i-1, j+1) ;(i+1, j-1);
   (i, j+1); (i+1, j); (i+1, j+1)]
  |> List.filter (fun (x, y) -> 0 <= x && x < lenx && 0 <= y && y < leny)

  |> List.fold_left (fun acc (x, y) ->
      if
      (match t.(i).(j), t.(x).(y) with
       | Int i, Char c -> c <> '.'
       | _, Char c -> (Printf.printf "char %c marche pas\n" c; false)
       | _ -> (Printf.printf "deux ints cote a cote\n"; false))
      then get_elt t.(i).(j)
      else acc
    ) 0

let solve() =
  let t = entry in
  let lenx = Array.length t
  and leny = Array.length t.(0) in
  List.init lenx (fun i -> List.init leny (fun j -> (i, j)))
  |> List.concat
  |> List.map (is_partn t)
  |> List.fold_left ( + ) 0
  |> Printf.printf "%i\n"

let _ =
  solve()
