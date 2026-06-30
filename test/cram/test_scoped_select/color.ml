type t = Red | Green | Blue

let get_column = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | s -> failwith ("bad color: " ^ s)

let get_column_nullable = function None -> None | Some s -> Some (get_column s)

let set_param = function Red -> "red" | Green -> "green" | Blue -> "blue"

let to_string = function Red -> "Red" | Green -> "Green" | Blue -> "Blue"
