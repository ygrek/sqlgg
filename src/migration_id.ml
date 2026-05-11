type t = { ord : int; name : string option }

let make ?name ord =
  { ord; name = (match name with Some "" | None -> None | n -> n) }

let parse raw =
  let s = String.trim raw in
  let prefix, name =
    String.index_opt s '_'
    |> Option.map_default
         (fun i -> String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))
         (s, None)
  in
  int_of_string_opt prefix |> Option.map (make ?name)

let to_string { ord; name } =
  name |> Option.map_default (Printf.sprintf "%d_%s" ord) (string_of_int ord)

let sort_key { ord; _ } =
  let canonical_stamp = "YYYYMMDDHHMMSS" in
  let s = string_of_int ord in
  let pad = String.length canonical_stamp - String.length s in
  int_of_string (if pad <= 0 then s else s ^ String.make pad '0')
