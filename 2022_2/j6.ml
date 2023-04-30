Printexc.record_backtrace true

let f = open_in "j6.txt"
let s = input_line f

let tous_different q =
  let q2 = Queue.copy q in
  let values = Array.init 14 (fun _ -> Queue.pop q2) in
  let res = ref true in
  for i = 0 to 13 do
    for j = i+1 to 13 do
      if values.(i) = values.(j) then res := false
    done;
  done;
  !res

let print_queue q = Queue.iter (fun x -> Printf.printf "%c" x) q; Printf.printf "\n"

exception Find of int
let sol1 () : int =
  let n = String.length s in
  let q = Queue.create() in
  for i = 0 to 12 do Queue.add s.[i] q done;
  try
    for i = 13 to n-1 do
      print_queue q;
      Queue.add s.[i] q;
      print_queue q;
      if tous_different q then begin Printf.printf "FIN"; raise (Find i) end;
      let _ = Queue.pop q in ();
      print_queue q; Printf.printf "\n"
    done;
    (-1)
  with Find x -> x

let _ =
  Printf.printf "Solution 1 :\n%i\n" (sol1 ())

(* 1
 *  too low :1815 (faut ajouter "1")*)
