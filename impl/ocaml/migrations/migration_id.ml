type t = {
  raw : string;
  ordinal : int64;
  name : string option;
  position : int64;
}

type error = [
  | `Invalid of string
  | `Too_long of int
]

let max_length = 191

let position ordinal =
  let digits = String.length (Int64.to_string ordinal) in
  let rec pad_zeros value remaining =
    if remaining <= 0 then
      value
    else
      pad_zeros (Int64.mul value 10L) (remaining - 1)
  in
  if digits < String.length "YYYYMMDD" then
    ordinal
  else
    pad_zeros ordinal (String.length "YYYYMMDDHHMMSS" - digits)

let parse raw =
  let length = String.length raw in
  if length > max_length then
    Error (`Too_long length)
  else
    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    in
    let is_name_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '-' -> true
      | char -> is_digit char
    in
    let prefix, name =
      match String.index_opt raw '_' with
      | None -> raw, None
      | Some separator ->
          let name_start = separator + 1 in
          ( String.sub raw 0 separator,
            Some (String.sub raw name_start (length - name_start)) )
    in
    let invalid = Error (`Invalid raw) in
    if prefix = "" || not (String.for_all is_digit prefix) then
      invalid
    else
      match name with
      | Some name
        when name = "" || not (String.for_all is_name_char name) ->
          invalid
      | None | Some _ -> (
          match Int64.of_string_opt prefix with
          | Some ordinal ->
              Ok { raw; ordinal; name; position = position ordinal }
          | None -> invalid)

let make ~ordinal ~name =
  parse
    (match name with
     | None -> Int64.to_string ordinal
     | Some name -> Int64.to_string ordinal ^ "_" ^ name)

let to_string { raw; _ } = raw

let ordinal { ordinal; _ } = ordinal

let name { name; _ } = name

let compare_position left right = Int64.compare left.position right.position

let next_position id =
  if id.position = Int64.max_int then
    Error `Position_overflow
  else
    Ok (Int64.succ id.position)

let error_to_string = function
  | `Invalid raw -> "invalid migration id: " ^ raw
  | `Too_long length ->
      "migration id is too long: "
      ^ string_of_int length
      ^ " bytes, the limit is "
      ^ string_of_int max_length

let parse_generated ~id =
  match parse id with
  | Ok parsed -> parsed
  | Error error -> invalid_arg ("sqlgg generated " ^ error_to_string error)
