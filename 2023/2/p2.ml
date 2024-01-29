Printexc.record_backtrace true

type color = Blue | Red | Green

let mB = 14
let mR = 12
let mG = 13

let match_c = function
  | "blue" -> Blue
  | "red" -> Red
  | "green" -> Green
  | _ -> failwith "match_c: unknown color"

let entry =
  List.init (read_int()) (fun _ ->
      read_line()
      |> String.split_on_char ':' |> (function [a; b] -> b | _ -> failwith "read_error")
      |> String.split_on_char ';'
      |> List.map (fun l -> String.split_on_char ',' l |> List.map (fun s -> Scanf.sscanf s " %i %s" (fun i c -> (i, match_c c))))
    )

let n = ref 0

let possible_game l =
  let possible_wave =
    List.for_all (function (nC, Blue) -> mB >= nC | (nC, Red) -> mR >= nC | (nC, Green) -> mG >= nC)
  in
  incr n;
  if List.for_all possible_wave l then (Some !n) else None

let solution() =
  entry
  |> List.map possible_game
  |> List.filter_map Fun.id
  |> List.fold_left (+) 0
  |> Printf.printf "%i\n"

let _ =
  solution()
