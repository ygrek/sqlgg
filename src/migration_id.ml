let sep = '_'
let sep_str = String.make 1 sep

module Name = struct

  type word = string

  let words s =
    s
    |> String.map (function ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') as c -> c | _ -> sep)
    |> String.split_on_char sep

  type action = { verb : word list; subject : word list }

  let action verb subject = { verb; subject }

  type phrase = { primary : string; fallbacks : string list }

  let word w = { primary = w; fallbacks = [] }

  let of_action { verb; subject } =
    let bare = String.concat sep_str verb in
    match subject with
    | [] -> word bare
    | _ -> { primary = bare ^ sep_str ^ String.concat sep_str subject; fallbacks = [ bare ] }

  type t = { head : word list; actions : action list }

  let make head actions = { head; actions }

  let phrases { head; actions } =
    List.map word head @ List.map of_action actions

  let join parts =
    String.concat sep_str (List.map (fun a -> a.primary) (phrases parts))

  let build ~limit parts =
    match phrases parts with
    | { primary = first; _ } :: rest when String.length first <= limit ->
      let buf = Buffer.create limit in
      Buffer.add_string buf first;
      let fits s = Buffer.length buf + 1 + String.length s <= limit in
      let add s = Buffer.add_char buf sep; Buffer.add_string buf s in
      let rec aux = function
        | [] -> ()
        | { primary; fallbacks } :: rest ->
          if fits primary then (add primary; aux rest)
          else Option.may add (List.find_opt fits fallbacks)
      in
      aux rest;
      Some (Buffer.contents buf)
    | _ -> None

  let render limit parts =
    match limit with
    | None -> join parts
    | Some n -> Option.default "" (build ~limit:n parts)

  let fit limit name = render limit { head = words name; actions = [] }

end

type t = { ord : int; name : string option }

let make ?name ord = { ord; name = (match name with Some "" -> None | n -> n) }

let name { name; _ } = name

let naming ~max_length ord =
  let spent = String.length (string_of_int ord) + String.length sep_str in
  Option.map (fun max -> max - spent) max_length

let is_digits s =
  s <> "" && String.for_all (function '0'..'9' -> true | _ -> false) s

let parse raw =
  match String.split_on_char sep (String.trim raw) with
  | prefix :: rest when is_digits prefix ->
    Option.map (make ~name:(String.concat sep_str rest)) (int_of_string_opt prefix)
  | _ -> None

let to_string { ord; name } =
  let prefix = string_of_int ord in
  Option.map_default (fun n -> prefix ^ sep_str ^ n) prefix name

let sort_key { ord; _ } =
  let date = String.length "YYYYMMDD" in
  let stamp = String.length "YYYYMMDDHHMMSS" in
  let digits = String.length (string_of_int ord) in
  let rec pad_zeros x n = if n <= 0 then x else pad_zeros (x * 10) (n - 1) in
  if digits < date then 0, ord else 1, pad_zeros ord (stamp - digits)

let compare_key (a1, b1) (a2, b2) =
  match Int.compare a1 a2 with 0 -> Int.compare b1 b2 | n -> n

let compare a b = compare_key (sort_key a) (sort_key b)

let next_ord id = snd (sort_key id) + 1
