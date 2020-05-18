open Core;;

(* Arrays *)

let numbers = [| 1; 2; 3 |]

let () = numbers.(0) <- 5

(* Mutable record fields *)

type person = {
    mutable name: string;
    mutable age: int;
    mutable salary: float;
}

let create_person () = { name = "Test"; age = 0; salary = 0. }
let update_person p (name, age, salary) =
    p.name <- name;
    p.age <- age;
    p.salary <- salary

(* Refs *)

let x = { contents = 0 }
let y = ref 1

let () = x.contents <- x.contents + 1
let () = y := !y + 1

let sum list =
    let acc = ref 0 in
    List.iter list ~f:(fun x -> acc := !acc + x);
    !acc
