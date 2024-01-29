open List

let six_from_j l j =
  List.init 6 (fun i -> List.init i (fun i -> if i+j < List.length l then List.nth l (i+j) else List.nth l 0 ))

let sub_lists l =
  List.mapi (fun j x -> six_from_j l j) l

let list_of_string = String.fold_left (fun a c -> c::a) []

let digits = List.map list_of_string ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let table = combine digits (List.init 8 (Int.succ))

let code_int c = 48 <= Char.code c && Char.code d.[0] <= 57

let get_calibration s =
  String.fold_left (fun a c -> c::a) [] s
  |> rev
  |> sub_lists
  |> filter (fun l -> List.mem digits l || (l <> [] && code_int (List.hd l) )  )


let _ =
  init (read_int()) (fun _ -> read_line())
  |>
