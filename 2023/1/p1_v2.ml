(* entry *)
List.init (read_int()) (fun _ -> read_line())

(* convert string to char list *)
|> List.map (fun s -> s |> String.fold_left (fun acc c -> c::acc) [] |> List.rev)

(* list of subseqs of char list *)
|> List.map (fun s ->
    (* from each char of the string *)
    List.map (fun s' ->
        (* set of subseqs where length < 6, the max digit length *)
        List.fold_left (fun (ss, acc) c ->
            if List.length ss <= 6 then (c::ss), ss::acc
            else (ss |> List.rev |> List.tl |> List.rev |> List.cons c) |> fun x -> x, x::acc
          ) ([], []) s'
        |> (fun (a, b) -> a::b) |> List.map List.rev
      )
      (* sub-lists from the beginning *)
      (List.fold_left (fun (ss, acc) x -> x::ss, (x::ss)::acc) ([], []) (List.rev s) |> snd)
    |> List.concat
  )
|> List.map (List.map (fun l -> l |> List.map Char.escaped |> List.fold_left (^) "")) (* on reconstitue les chaines pour + facilement comparer avec les strings *)
(* filter numbers and tokens, we don't care about repetitions *)
|> List.map (List.filter (function d when d <> "" && String.length d = 1 && (Char.code d.[0] >= 48 && Char.code d.[0] <= 57) -> true
                       | "one" | "two" | "three" | "four" | "five" | "six" | "seven" | "eight" | "nine" -> true
                       | _ -> false))
(* filter first and last *)
|> List.map (fun l -> List.filteri (fun i x -> i = 0 || i+1 = List.length l) l)
|> List.map (List.map (function | "one" -> 1 | "two" -> 2 | "three" -> 3 | "four" -> 4 | "five" -> 5 | "six" -> 6 | "seven" -> 7 | "eight" -> 8 | "nine" -> 9 | s -> int_of_string s))
|> List.map (function l -> 10*(List.nth l 0) + (List.nth l (List.length l -1)))
|> List.fold_left (+) 0
|> Printf.printf "%i\n"
