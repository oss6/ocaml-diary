type 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

let leaf a = Node (a, Empty, Empty)

let root = function
    | Empty          -> failwith "root"
    | Node (x, _, _) -> x
