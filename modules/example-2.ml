open Base

(* Interface *)
module type ID = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val (=) : t -> t -> bool
end

(* Implementation *)
module String_id = struct
    type t = string
    let of_string x = x
    let to_string x = x
    let (=) = String.(=)
end

(* Two modules using the same implementation *)
module Username : ID = String_id
module Hostname : ID = String_id

(* Local opens *)
(* let average x y =
    let open Int64 in
    (x + y) / of_int 2 *)

(* Rebind the name of a module *)
let uppercase_and_filter l =
    let module L = List in
    let filtered = L.filter l ~f:(fun x -> String.length x >= 3) in
    L.map filtered ~f:String.uppercase

(* Including modules *)
module Interval = struct
    type t =
        | Empty
        | Interval of int * int
    
    let create low high =
        if high < low then Empty else Interval (low, high)
end

module Extended_interval = struct
    include Interval

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) -> x >= low && x <= high
end

(*
    If implementations come after include
    they may shadow existing implementation in the included module.
*)