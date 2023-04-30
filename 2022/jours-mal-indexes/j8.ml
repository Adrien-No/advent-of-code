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
  balay_from_left(); print_visible_matrix visibility;
  balay_from_down(); print_visible_matrix visibility;
  balay_from_up(); print_visible_matrix visibility;
  balay_from_right(); print_visible_matrix visibility;
  Array.fold_left (fun b l -> b + Array.fold_left (+) 0 (Array.map (fun x -> if x = true then 1 else 0) l)) 0 visibility

let part2 () =
  let c = Array.length data in
  let data2 = Array.make_matrix c c 0 in
  for i = 0 to c-1 do
    for j = 0 to c-1 do


      (* from left *)
      let scenic_l = ref 1 in
      let h = ref data.(i).(j) in
      let not_ft = ref false in
      for j2 = j+1 to c-1 do
        if data.(i).(j2) > !h then
          begin
            if !not_ft then incr scenic_l else not_ft := true;
            h := data.(i).(j2)
          end;
      done;


      (* from down*)
      let scenic_d = ref 1 in
      let h = ref data.(i).(j) in
      let not_ft = ref false in
      for i2 = c-1 downto i+1 do

        if data.(i2).(j) > !h then
          begin
            if !not_ft then incr scenic_d else not_ft := true;
            h := data.(i2).(j)
          end;

      done;

        (* from up *)
      let scenic_u = ref 1 in
      let h = ref data.(i).(j) in
      let not_ft = ref false in
      for i2 = i+1 to c-1 do
        if data.(i2).(j) > !h then
          begin
            if !not_ft then incr scenic_u else not_ft := true;
            h := data.(i2).(j)
          end
      done;

      (* from right *)
      let scenic_r = ref 1 in
      let h = ref data.(i).(j) in
      let not_ft = ref false in
      for j2 = c-1 downto j+1 do
        if data.(i).(j2) > !h then
          begin
            if !not_ft then incr scenic_r else not_ft := true;
            h := data.(i).(j2)
          end
      done;

      let prod = !scenic_u * !scenic_d * !scenic_l * !scenic_r in
      data2.(i).(j) <- prod;
      Printf.printf "t(%i,%i) =%i | %i*%i*%i*%i = %i\n\n" i j data.(i).(j) !scenic_u !scenic_d !scenic_l !scenic_r prod
    done
  done;
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
