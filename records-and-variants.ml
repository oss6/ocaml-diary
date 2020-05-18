(* 
    Record types
*)

type point2d = { x: float; y: float }

let coords = { x: 2.4; y: 5. }

(* Pattern match on record *)
let add_coords_1 { x = x_coord; y = y_coord } = x_coord +. y_coord

let add_coords_2 { x; y } = x +. y

let add_coords_3 c = c.x +. c.y

(*
    Variants
*)

type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower_left: point2d; width: float; height: float }
type segment_desc = { p1: point2d; p2: point2d }

type scene_element =
    | Circle of circle_desc
    | Rect of rect_desc
    | Segment of segment_desc
