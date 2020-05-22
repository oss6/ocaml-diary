open Core

let padr_s l s =
    let n = abs (String.length s - l) in
    s ^ String.make n ' '

let tabulate header table =    
    (* Length calculations *)
    let lengths = List.map (header :: table) ~f:(fun xs -> List.map xs ~f:String.length) in
    let max_lengths = List.fold (List.tl_exn lengths) ~init:(List.hd_exn lengths) ~f:(List.map2_exn ~f:Int.max) in
    
    (* Header *)
    let padded_header = List.map2_exn max_lengths header ~f:padr_s in
    let printed_header = "| " ^ String.concat ~sep:" | " padded_header ^ " |\n" in
    
    (* Divider *)
    let divider = "| " ^ (String.make ((List.fold max_lengths ~init:0 ~f:(+)) + List.length max_lengths * 2) '-') ^ " |\n" in
    
    (* Table rows *)
    let padded_rows = List.map table ~f:(List.map2_exn max_lengths ~f:padr_s) in
    let rows = List.map padded_rows ~f:(fun r -> "| " ^ String.concat ~sep:" | " r ^ " |") in
    let printed_rows = String.concat ~sep:"\n" rows in

    printed_header ^ divider ^ printed_rows

let example = tabulate
["language";"architect";"first release"]
[ ["Lisp" ;"John McCarthy" ;"1958"] ;
["C" ;"Dennis Ritchie";"1969"] ;
["ML" ;"Robin Milner" ;"1973"] ;
["OCaml";"Xavier Leroy" ;"1996"]]