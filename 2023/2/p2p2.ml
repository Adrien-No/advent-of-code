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

let possible_game l =
  let possible_wave l =
    List.fold_left (fun (x,y,z) (nC, c) ->
        match c with
        | Blue -> (max x nC, y, z)
        | Red -> (x, max y nC, z)
        | Green -> (x, y, max z nC)
      ) (0, 0, 0) l
  in

  List.map possible_wave l
  |> List.fold_left (fun (x, y, z) (x', y', z') -> (max x x', max y y', max z z')) (0, 0, 0)
  |> fun (x, y, z) -> x*y*z

let solution() =
  entry
  |> List.map possible_game
  |> List.fold_left (+) 0
  |> Printf.printf "%i\n"

let _ =
  solution()
