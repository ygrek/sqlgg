type t =
  | Text of string
  | Numbered_param
  | Concat of t list

let text s = Text s
let numbered_param = Numbered_param
let concat l = Concat l

let render t =
  let b = Buffer.create 128 in
  let rec go next = function
    | Text s -> Buffer.add_string b s; next
    | Numbered_param -> Buffer.add_char b '$'; Buffer.add_string b (string_of_int next); next + 1
    | Concat l -> List.fold_left go next l
  in
  let (_ : int) = go 1 t in
  Buffer.contents b
