Printexc.record_backtrace true

let f = open_in "j8.txt"
(*Printf.printf "%i-eme ligne =" i; print_int_array data.(i);*)
(*Printf.printf "t(%i,%i) = %i | valeur = %s\n" i j data.(i).(j) (val_bool visibility.(i).(j));*)
let (data : int array array) =
  let rec aux big_l =
    try
      let s = input_line f in
      let l = String.fold_left (fun b x -> (x |> Char.escaped |> int_of_string) :: b) [] s in
      aux (l::big_l)
    with End_of_file -> big_l
  in
  let l = aux [] in
  (*let c = List.length l in*)
  let l = List.fold_left (fun b x -> (x |> List.rev |> Array.of_list ) :: b) [] l in
  Array.of_list l

let print_int_array l = l |> Array.iter (Printf.printf "%i"); Printf.printf "\n"

let print_int_matrix m =
  Array.iter (fun t -> Array.iter (Printf.printf "%i") t; Printf.printf "\n") m;Printf.printf "\n\n"

let print_visible_matrix m =
  Array.iter (fun t -> Array.iter (fun x -> if x then Printf.printf "X" else Printf.printf " ") t; Printf.printf "\n") m;Printf.printf "\n\n"

let val_bool b = if b then "true" else "false"

let part1 () =
  let c = Array.length data in
  let visibility = Array.make_matrix c c false in


  let balay_from_left () =
    for i = 0 to c-1 do
      let h = ref (-1) in
      (*Printf.printf "%i-eme ligne =" i; print_int_array data.(i);*)
      for j = 0 to c-1 do

        if data.(i).(j) > !h then
          begin
            visibility.(i).(j) <- true;
            h := data.(i).(j)
          end;

      done
    done
  in
  let balay_from_down () =
    for j = 0 to c-1 do
      let h = ref (-1) in
    for i = c-1 downto 0 do


        if data.(i).(j) > !h then
          begin
            visibility.(i).(j) <- true;
            h := data.(i).(j)
          end;

      done

    done
  in

  let balay_from_up () =

    for j = 0 to c-1 do
      let h = ref (-1) in
      for i = 0 to c-1 do
        if data.(i).(j) > !h then
          begin
            visibility.(i).(j) <- true;
            h := data.(i).(j)
          end
      done
    done
  in
    let balay_from_right () =

      for i = 0 to c-1 do
        let h = ref (-1) in
      for j = c-1 downto 0 do


        if data.(i).(j) > !h then
          begin
            visibility.(i).(j) <- true;
            h := data.(i).(j)
          end
      done
    done
  in
  print_int_matrix data;
  balay_from_left(); (*print_visible_matrix visibility;*)
  balay_from_down(); (*print_visible_matrix visibility;*)
  balay_from_up(); (*print_visible_matrix visibility;*)
  balay_from_right(); (*print_visible_matrix visibility;*)
  Array.fold_left (fun b l -> b + Array.fold_left (+) 0 (Array.map (fun x -> if x = true then 1 else 0) l)) 0 visibility

let part2 () =
  let c = Array.length data in
  let data2 = Array.make_matrix c c 0 in
  for i = 1 to c-2 do
    for j = 1 to c-2 do
      let h = data.(i).(j) in

      (* from top *)
      let i2 = 1 in
      let rec aux i2 =
        if i+i2 = 0 || i+i2 = c-1 || data.(i+i2).(j) >= h
        then i2
        else aux (i2+1)
      in
      let top_scenic = aux i2 in

      (* from bot *)
      let i2 = 1 in
      let rec aux i2 =
        if i-i2 = 0 || i-i2 = c-1 || data.(i-i2).(j) >= h
        then i2
        else aux (i2+1)
      in
      let bot_scenic = aux i2 in

      (* from left *)
      let j2 = 1 in
      let rec aux j2 =
        if j+j2 = 0 || j+j2 = c-1 || data.(i).(j+j2) >= h
        then j2
        else aux (j2+1)
      in
      let left_scenic = aux j2 in

      (* from right *)
      let j2 = 1 in
      let rec aux j2 =
        if j-j2 = 0 || j-j2 = c-1 || data.(i).(j-j2) >= h
        then j2
        else aux (j2+1)
      in
      let right_scenic = aux j2 in

      let score = top_scenic*bot_scenic*left_scenic*right_scenic in
      data2.(i).(j) <- score;
      Printf.printf "t(%i,%i) =%i | %i*%i*%i*%i = %i\n\n" i j data.(i).(j) top_scenic left_scenic right_scenic bot_scenic score

    done;
  done;
  print_int_matrix data2;
  Array.fold_left (fun b t -> max (Array.fold_left max 0 t) b) 0 data2

let _ =
  Printf.printf "Partie 1:%i\n" (part1());
  Printf.printf "Partie 2:%i" (part2())

(* part 2 :840 too low *)

(*
30373
25512
65332
33549
35390

 *)

(*
 *       (* from up *)
      let j2 = ref (j) in
      if j <> 0 && j <> c-1 && !i2 <> 0 && !i2 <> c-1 then
        begin
          incr i2;
      while data.(!i2).(j) < h && j <> 0 && j <> c-1 && !i2 <> 0 && !i2 <> c-1 do
        incr i2
      done;
      end;
      let scenic_u = i in

      (* from left *)
      let left j2 =
        if j2 = 0 || j2 = c-1 || i = 0 || i = c-1 then 0 else
        let rec aux j2 =
          if data.(i).(j2) < h then aux (j2+1)
          else j2
        in
        aux j2
      in
      let scenic_l = left j in

      (* from down*)
      let i2 = ref (i) in
      while data.(j).(!i2) < h && j <> 0 && j <> c-1 && !i2 <> 0 && !i2 <> c-1 do
        decr i2
      done;
      let scenic_d = i - !i2 in

      (* from right *)
      let j2 = ref (j) in
      while data.(!j2).(i) < h && !j2 <> 0 && !j2 <> c-1 && i <> 0 && i <> c-1 do
        decr j2
      done;
      let scenic_r = j - !j2 in

      let prod = scenic_u * scenic_d * scenic_l * scenic_r in
      data2.(i).(j) <- prod;
      Printf.printf "t(%i,%i) =%i | %i*%i*%i*%i = %i\n\n" i j data.(i).(j) scenic_u scenic_l scenic_r scenic_d prod
    done
  done; *)
