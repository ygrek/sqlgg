type t = [`A of int | `B of string | `Z of t]

let rec use_t = function
  | `A i -> i
  | `B s -> String.length s
  | `Z x -> use_t x

let rec show_t 
  : t -> unit (* comment this *)
  = function 
  | `A i -> print_endline "A"
  | `B s -> print_endline "B"
  | `Z x -> (print_string "Z"; show_t x)

let rec make_narrow = function
  | `A i -> `A i
  | `B s -> `A (String.length s)
  | `Z x -> `Z (make_narrow x)

let rec use_narrow = function
  | `A i -> i
  | `Z x -> (use_narrow x)

let tee f x = f x; x

let final x = 
  let y = make_narrow x in
  show_t y;
  use_narrow y

let final x = use_narrow (make_narrow (tee show_t x))

let final x = 
  use_narrow (tee (fun (z:[`A of int| `Z of 'a] as 'a) -> show_t (z:>t)) (make_narrow x))
