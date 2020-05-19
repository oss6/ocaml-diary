open Core

(* fun *)

let add = fun x -> fun y -> x + y
let add' x y = x + y

(* function keyword *)

let some_or_default def = function
    | Some x -> x
    | None   -> def

let list_with_defaults = List.map ~f:(some_or_default 0) [Some 3; Some 4; None; Some 8; None]

(* Labeled arguments *)

let ratio ~num ~denom = Float.of_int num /. Float.of_int denom

let ratio_value = ratio ~num:3 ~denom:10

(* Optional arguments *)

let concat ?sep x y =
    let sep = match sep with
    | None   -> ""
    | Some x -> x
    in x ^ sep ^ y

let concat' ?(sep="") x y = x ^ sep ^ y

let concat_value1 = concat "foo" "bar"
let concat_value2 = concat ~sep:"-" "foo" "bar"

(* What if concat's default changes? We need to change uppercase_concat's default as well *)
let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b

(* A solution to the above (explicit passing of argument to default parameter) *)
let uppercase_concat' ?sep a b = concat ?sep (String.uppercase a) b

