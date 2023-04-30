let file = open_in "puzzle.txt"

let rec read f l1 l2 =
    try
        let line = input_line f in
                match line with
                |"\n"-> read f l1 (l1::l2)
                | _ -> read f (Stdlib.int_of_string line::l1) l2
    with End_of_file -> []

let _ =
