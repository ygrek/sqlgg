type t = {
  ordinal : int;
  name : string option;
}

type error = [
  | `Invalid of string
  | `Too_long of int
]

let max_length = 191

let parse raw =
  let value = String.trim raw in
  let length = String.length value in
  if length > max_length then
    Error (`Too_long length)
  else
    match String.split_on_char '_' value with
    | prefix :: rest
      when prefix <> ""
           && String.for_all
                (function
                  | '0' .. '9' -> true
                  | _ -> false)
                prefix ->
        (match int_of_string_opt prefix with
         | Some ordinal ->
             Ok
               {
                 ordinal;
                 name =
                   (match String.concat "_" rest with
                    | "" -> None
                    | name -> Some name);
               }
         | None -> Error (`Invalid raw))
    | _ -> Error (`Invalid raw)

let to_string { ordinal; name } =
  let prefix = string_of_int ordinal in
  match name with
  | None -> prefix
  | Some name -> prefix ^ "_" ^ name

let compare left right =
  let sort_key ordinal =
    let digits = String.length (string_of_int ordinal) in
    let rec pad_zeros value remaining =
      if remaining <= 0 then
        value
      else
        pad_zeros (value * 10) (remaining - 1)
    in
    if digits < String.length "YYYYMMDD" then
      0, ordinal
    else
      1, pad_zeros ordinal (String.length "YYYYMMDDHHMMSS" - digits)
  in
  match sort_key left.ordinal, sort_key right.ordinal with
  | (left_kind, left_ordinal), (right_kind, right_ordinal) ->
      (match Int.compare left_kind right_kind with
       | 0 -> Int.compare left_ordinal right_ordinal
       | ordering -> ordering)

let error_to_string = function
  | `Invalid raw -> "invalid migration id: " ^ raw
  | `Too_long length ->
      "migration id is too long: "
      ^ string_of_int length
      ^ " bytes, the limit is "
      ^ string_of_int max_length
