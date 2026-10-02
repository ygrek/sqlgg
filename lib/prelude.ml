
exception At of ((int * int) * exn)
let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"

let const c _ = c
let flip f x y = f y x

let tuck l x = l := x :: !l
let option_list = function Some x -> [x] | None -> []

let hashtbl_restore h s = Hashtbl.clear h; Hashtbl.iter (Hashtbl.replace h) s

module String_map = struct
  include Map.Make (String)

  let to_json value_to_json map =
    `Assoc (List.map (fun (key, value) -> key, value_to_json value) (bindings map))

  let of_json value_of_json = function
    | `Assoc fields ->
      of_seq (List.to_seq (List.map (fun (key, value) -> key, value_of_json value) fields))
    | json -> Jsonkit.of_json_error ~json "expected a JSON object"

  let t_jsonschema value_jsonschema =
    `Assoc [ "type", `String "object"; "additionalProperties", value_jsonschema ]
end

module String_set = struct
  include Set.Make (String)

  let to_json set = Jsonkit.Primitives.(list_to_json string_to_json) (elements set)
  let of_json json = of_list (Jsonkit.Primitives.(list_of_json string_of_json) json)
  let t_jsonschema = Jsonkit.Primitives.(list_jsonschema string_jsonschema)
end

let unique_by ~key l =
  let (_, kept) =
    List.fold_left (fun ((seen, kept) as acc) x ->
      let k = key x in
      if String_set.mem k seen then acc else (String_set.add k seen, x :: kept))
      (String_set.empty, []) l
  in
  List.rev kept

let fail fmt = Printf.ksprintf failwith fmt
let failed ~at fmt = Printf.ksprintf (fun s -> raise (At (at, Failure s))) fmt
let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
