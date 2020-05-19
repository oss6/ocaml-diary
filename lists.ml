open Core

let empty = function
    | []     -> true
    | _ :: _ -> false

let hd = function
    | []     -> failwith "hd"
    | x :: _ -> x

let tl = function
    | []      -> failwith "tl"
    | _ :: xs -> xs

let rec nth = function
    | 0, x :: _  -> x
    | n, _ :: xs -> nth (n - 1, xs)
    | _, _       -> failwith "nth"

let rec append xs ys = match xs with
    | []      -> ys
    | x :: xs -> x :: (append xs ys)

let rec rev = function
    | []      -> []
    | x :: xs -> append (rev xs) [x]

let rec (===) xs ys = match (xs, ys) with
    | [], []              -> true
    | [], _::_ | _::_, [] -> false
    | x :: xs, y :: ys    -> x = y && xs === ys

let rec zip = function
    | [], []                  -> []
    | x :: xs, y :: ys        -> (x, y) :: zip (xs, ys)
    | _ :: _, [] | [], _ :: _ -> failwith "zip"

let rec unzip = function
    | []           -> [], []
    | (x, y) :: xs -> let (a, b) = unzip xs in x :: a, y :: b

let rec range a b = if a = b then [a] else a :: range (a + 1) b

let rec member x = function
    | []      -> false
    | y :: ys -> x = y || member x ys

let rec filter p = function
    | []      -> []
    | x :: xs -> if p x then x :: filter p xs else filter p xs

let rec partition p = function
    | [] -> [], []

let splitn n l =
    let rec splitn' acc = function
        | _, []      -> [], []
        | 0, s       -> (acc, s)
        | n, x :: xs -> splitn' acc @ [x] (n - 1, xs)
    in splitn' [] (n, l)

let uppercase_first_entry line =
    match String.split ~on:',' line with
    | [] -> failwith "uppercase_first_entry"
    | x :: xs -> String.concat ~sep:"," (String.uppercase x :: xs)