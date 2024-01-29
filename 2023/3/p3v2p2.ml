open Array

let t = init (read_int()) (fun _ -> read_line()|> String.to_seq |> of_seq)

let lenx = length t
let leny = length t.(0)

let numbers = make_matrix lenx leny 0

let treat_number l (i, j0) =
  let s = ref 0 in
  let j = ref j0 in
  while !j < leny && Char.code l.(!j) >= 48 && Char.code l.(!j) <= 57 do
    s := 10 * !s + (l.(!j) |> Char.escaped |> int_of_string);
    incr j
  done;
  for j = j0 to !j-1 do
    numbers.(i).(j) <- !s
  done;
  max (j0+1) !j

let treat_numbers_line i =
  let j = ref 0 in

  while !j < leny do
    j := (treat_number t.(i) (i, !j))
  done

let solve() =

  for i = 0 to lenx-1 do
    treat_numbers_line i
  done;

  let res = ref 0 in

  for i = 0 to lenx-1 do
    for j = 0 to leny-1 do
      if t.(i).(j) = '*' then
        let adjacents =
          [(i-1, j-1); (i-1, j); (i, j-1);
           (i-1, j+1) ;(i+1, j-1);
           (i, j+1); (i+1, j); (i+1, j+1)]
          |> List.filter (fun (x, y) -> 0 <= x && x < lenx && 0 <= y && y < leny && numbers.(x).(y) <> 0)
          |> (fun l -> Printf.printf "len:%i\n" (List.length l); l)
          |> List.map (fun (x, y) -> numbers.(x).(y))
          |> List.sort_uniq compare
        in
        res := !res +
               match adjacents with
               | [a; b] -> a*b
               | _ -> 0
    done;
  done;
  Printf.printf "%i\n" !res

let _ = solve()
