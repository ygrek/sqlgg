type json = [
  | `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Assoc of (string * json) list
  | `List of json list
  | `Int of int
  | `Intlit of string
]

type json_path = Sqlgg_json_path.Ast.t
type one_or_all = [ `One | `All ]

 let rec convert_json = function
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as x -> x
  | `Assoc assoc_list ->
    let convert_pair (key, value) = (key, convert_json value) in
    `Assoc (List.map convert_pair assoc_list)
  | `List json_list | `Tuple json_list ->
    `List (List.map convert_json json_list)
  | `Variant _ -> failwith "Variant type is not supported"
