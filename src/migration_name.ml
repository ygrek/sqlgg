let separator = '_'
let separator_string = String.make 1 separator

type word = string

let words s =
  s
  |> String.map (function
    | ('a'..'z' | 'A'..'Z' | '0'..'9') as c -> c
    | _ -> separator)
  |> String.split_on_char separator

type action = Fixed of string | Verb_subject of { verb : string; subject : string }

let action verb subject =
  match subject with
  | [] -> Fixed (String.concat separator_string verb)
  | _ ->
      Verb_subject {
        verb = String.concat separator_string verb;
        subject = String.concat separator_string subject;
      }

type t = { head : word list; actions : action list }

let make head actions = { head; actions }

let render limit { head; actions } =
  let whole = function
    | Fixed s -> s
    | Verb_subject { verb; subject } ->
        verb ^ separator_string ^ subject
  in
  match List.map (fun w -> Fixed w) head @ actions with
  | first :: rest when String.length (whole first) <= limit ->
    let buf = Buffer.create limit in
    Buffer.add_string buf (whole first);
    let fits s = Buffer.length buf + 1 + String.length s <= limit in
    let add s =
      Buffer.add_char buf separator;
      Buffer.add_string buf s
    in
    let rec aux = function
      | [] -> ()
      | piece :: rest ->
        let s = whole piece in
        if fits s then (add s; aux rest)
        else (match piece with
          | Verb_subject { verb; _ } when fits verb -> add verb
          | Fixed _ | Verb_subject _ -> ())
    in
    aux rest;
    Some (Buffer.contents buf)
  | _ -> None

let limit ~max_id_length ordinal =
  let ordinal_length = String.length (Int64.to_string ordinal) in
  if ordinal_length > max_id_length then Error `Ordinal_does_not_fit
  else Ok (max_id_length - ordinal_length - String.length separator_string)
