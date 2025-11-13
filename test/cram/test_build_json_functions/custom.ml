type t = {
  a: string;
  b: int64;
}

let of_yojson (x : Sqlgg_trait_types.json) =
  match x with
  | `Assoc lst ->
    let a =
      match List.assoc_opt "a" lst with
      | Some (`String s) -> s
      | _ -> failwith "Invalid or missing field 'a'"
    in
    let b =
      match List.assoc_opt "b" lst with
      | Some (`Int i) -> Int64.of_int i
      | Some (`Intlit s) -> Int64.of_string s
      | _ -> failwith "Invalid or missing field 'b'"
    in
    { a; b }
  | _ -> failwith "Expected JSON object"

let to_yojson t =
  `Assoc [
    "a", `String t.a;
    "b", `Int (Int64.to_int t.b);
  ]

let get_column_nullable = function
  | Some json -> Some (of_yojson json)
  | None -> None

let set_param x =
  let x = Sqlgg_trait_types.convert_json (to_yojson x) in
  (x :> Sqlgg_trait_types.json)
