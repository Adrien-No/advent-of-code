let is_digit = function
  | "one" -> Some 1
  | "two" -> Some 2
  | "three" -> Some 3
  | "four" -> Some 4
  | "five" -> Some 5
  | "six" -> Some 6
  | "seven" -> Some 7
  | "eight" -> Some 8
  | "nine" -> Some 9
  | _ -> None

let blit_s i j s =
  (* j exclude *)
  let res = ref "" in
  for k = i to (i+j)-1 do
    res := !res ^ (Char.escaped s.[k])
  done;
  !res

exception Bad
let read =
  let l = List.init (read_int()) (fun _ -> read_line()) in


  let rec parse s i =
    let len = String.length s in
    if i >= len then [] else
      let c = s.[i] in
    if Char.code c >= 48 && Char.code c <= 57 then (c |> Char.escaped |> int_of_string) :: parse s (i+1) else

    (* replace digits *) (* jump useless finally *)
    let jump, meaning =
    try
      if i+2 >= len then raise Bad;
      match is_digit (blit_s i 3 s) with
        Some n -> 3, n
      | _ -> if i+3 >= len then raise Bad;
        match is_digit (blit_s i 4 s) with
        | Some n -> 4, n
        | _ -> if i+4 >= len then raise Bad;
          match is_digit (blit_s i 5 s) with
          | Some n -> 5, n
          | _ -> raise Bad

    with Bad -> 1, -1
    in
    if meaning = -1 then parse s (i+1)
    else meaning :: parse s (i+1)
  in
  List.map (fun s -> parse s 0) l

let calibrate s =
  (* let len = List.length s in*)
  10 * (List.hd s) + (List.fold_left (fun acc x -> x) (List.hd s) s)

let compute1 l =
  l
  |> List.map calibrate
  |> List.fold_left (+) 0
  |> Printf.printf "p1: %i\n"

let _ =
  compute1 read
