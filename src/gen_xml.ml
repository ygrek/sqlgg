(** XML generation *)

open Printf
open ExtLib
open Sqlgg
open Prelude

open Stmt
open Gen

type xml = | Node of (string * (string * string) list * xml list)
           | Comment of string

let xml_escape s =
  let b = Buffer.create 10 in
  let add s = Buffer.add_string b s in
  String.iter (function
  | '&' -> add "&amp;"
  | '"' -> add "&quot;"
  | '\n' -> add "&#x0A;"
  | '\r' -> ()
  | '<' -> add "&lt;"
  | '>' -> add "&gt;"
  | c -> Buffer.add_char b c) s;
  Buffer.contents b

let xml_to_string xml =
  let b = Buffer.create 1000 in
  let rec iter spaces = function
    | Node (name,attrs,children) ->
        bprintf b "\n%s<%s" spaces name;
        List.iter (fun (n,v) -> bprintf b " %s=\"%s\"" n (xml_escape v)) attrs;
        begin match children with
        | [] -> bprintf b "/>"
        | _ -> bprintf b ">"; List.iter (iter (spaces ^ " ")) children; bprintf b "\n%s</%s>" spaces name
        end
    | Comment text -> bprintf b "\n<!-- %s -->" (Gen_caml.replace_all ~str:text ~sub:"--" ~by:"&mdash;")
  in
  iter "" xml;
  Buffer.contents b

(*
let _ =
  Node ("test",["name","d\"s&quot;ds"],[]) |> xml_to_string |> print_endline
*)

let comment (x,_) fmt = Printf.ksprintf (fun s -> x := Comment s :: !x) fmt
let empty_line _ = ()

let value ?(inparam=false) ?(tuplelist=false) v =
  let attrs =
    List.concat
      [
        ["name",v.vname; "type",v.vtyp];
        if inparam then ["set","true"] else [];
        if tuplelist then ["tuplelist","true"] else [];
        if v.nullable then ["nullable","true"] else [];
      ]
  in
  Node ("value", attrs, [])

(* open Gen_caml.L *)
open Gen_caml.T

let params_to_values = List.map value $ all_params_to_values
let inparams_to_values = List.map (value ~inparam:true) $ all_params_to_values
let tuplelist_params_to_values = List.map (value ~tuplelist:true) $ all_params_to_values
let schema_to_values = List.map value $ schema_to_values

type t = xml list ref * xml list ref

let start () = ref [], ref []

let get_sql_string stmt =
  let rec map i = function
  | Static s -> s
  | SubstIn param -> "@@" ^ show_param_name param i (* TODO join text and prepared params earlier for single indexing *)
  | SubstTuple (id, _) -> "@@@" ^ make_param_name i id
  | DynamicIn (_p, _, sqls) -> String.concat "" @@ List.map (map 0 ) sqls
  | Dynamic _ -> fail "dynamic choice not supported for xml output"
  in
  String.concat "" @@ List.mapi map @@ get_sql stmt

let generate_code (x,_) index stmt =
  let name = choose_name stmt.props stmt.kind index in
  let input =
    Node ("in",[],
          (tuplelist_params_to_values @@ tuplelist_params_only stmt.vars) @
          (params_to_values @@ params_only stmt.vars) @ (inparams_to_values @@ inparams_only stmt.vars)) in
  let output = Node ("out",[],schema_to_values stmt.schema) in
  let sql = get_sql_string stmt in
  let attrs =
    match stmt.kind with
    | Select `Nat      -> ["kind", "select"; "cardinality", "n"]
    | Select `Zero_one -> ["kind", "select"; "cardinality", "0,1"]
    | Select `One      -> ["kind", "select"; "cardinality", "1"]
    | Insert (_, t)    -> ["kind", "insert"; "target", Sql.show_table_name t; "cardinality", "0"]
    | Create t         -> ["kind", "create"; "target", Sql.show_table_name t; "cardinality", "0"]
    | CreateIndex t    -> ["kind", "create_index"; "target",t;"cardinality","0"]
    | Update None      -> ["kind", "update"; "cardinality", "0"]
    | Update (Some t)  -> ["kind", "update"; "target", Sql.show_table_name t; "cardinality", "0"]
    | Delete t         -> ["kind", "delete"; "target", String.concat "," @@ List.map Sql.show_table_name t; "cardinality", "0"]
    | Alter t          -> ["kind", "alter"; "target", String.concat "," @@ List.map Sql.show_table_name t; "cardinality", "0"]
    | Drop t           -> ["kind", "drop"; "target", Sql.show_table_name t; "cardinality", "0"]
    | CreateRoutine s  -> ["kind", "create_routine"; "target", s]
    | Other            -> ["kind", "other"]
  in
  let nodes = [ input; output] in
  x := Node ("stmt", ("name",name)::("sql",sql)::("category",show_category @@ category_of_stmt_kind stmt.kind)::attrs, nodes) :: !x

let generate_table (x,_) (name,schema) =
  x := Node ("table", ["name",Sql.show_table_name name], [Node ("schema",[],schema_to_values schema)]) :: !x

let start_output (x,pre) = pre := !x; x := []

let finish_output (x,pre) =
  print_endline "<?xml version=\"1.0\"?>";
  List.iter (fun z -> z |> xml_to_string |> print_endline) (List.rev !pre);
  Node ("sqlgg",[],List.rev !x) |> xml_to_string |> print_endline;
  x := [];
  pre := []

let generate out _ stmts =
  start_output out;
  List.iteri (generate_code out) stmts;
  List.iter (generate_table out) (Tables.all ());
  finish_output out
