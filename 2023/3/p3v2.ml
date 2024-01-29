Printexc.record_backtrace true

open Array

let t = init (read_int()) (fun _ -> read_line()|> String.to_seq |> of_seq)

let lenx = length t
let leny = length t
let marked = init lenx (fun _ -> init leny (fun _ -> false))

let find_number l (i,j) =
  let mark = ref false in
  let j = ref j in
  let s = ref 0 in
  while !j < leny && Char.code l.(!j) >= 48 && Char.code l.(!j) <= 57 do
    if marked.(i).(!j) then mark := true;

    s := (10 * !s) + (l.(!j) |> Char.escaped |> int_of_string);

    incr j
  done;
  if !mark then (!s, !j) else ( (* Printf.printf "pas marqué\n"; *) (0, !j))

let find_numbers l i =
  let j = ref 0
  and s = ref 0 in
  while !j < leny do
    let sj, new_j = find_number l (i, !j) in
    s := !s + sj;
    j := new_j+1
  done;
  !s

let _ =
  let res = ref 0 in

  (* marquage *)
  for i = 0 to lenx-1 do
    for j = 0 to leny-1 do
      let mark = ref false in
      for x = (max 0 (i-1)) to (min (lenx-1) (i+1)) do
        for y =  (max 0 (j-1)) to (min (leny-1) (j+1)) do
          let c = t.(x).(y) in
          (*Printf.printf "%c\n" c;*)
          if (Char.code c < 48 || Char.code c > 57) && c <> '.' then mark := true
        done;
      done;
      if !mark then marked.(i).(j) <- true
    done;
  done;

  (* on extrait la valeur *)
  for i = 0 to lenx-1 do
    res := !res + (find_numbers t.(i) i)
  done;
  Printf.printf "%i\n" !res
