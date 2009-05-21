(* Java code generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

module G = Gen_cxx

let comment = G.comment
let empty_line = G.empty_line

let start_class name =
   output "public class %s" name;
   G.open_curly ()

let end_class name =
   G.close_curly " // class %s" name;
   empty_line ()

let set_column attr index =
  output "Traits::set_column_%s(stmt, %u, obj.%s);" 
    (Type.to_string attr.RA.domain)
    index
    (name_of attr index)

let get_column attr index =
  output "Traits::get_column_%s(stmt, %u, obj.%s);" 
    (Type.to_string attr.RA.domain)
    index
    (name_of attr index)

let param_type_to_string t = Option.map_default Type.to_string "Any" t
let as_cxx_type str = "typename Traits::" ^ str

let set_param index param =
  let (id,t) = param in
  output "Traits::set_param_%s(stmt, %s, %u);" 
    (param_type_to_string t)
    (param_name_to_string id index)
    index

(*
public interface ServerCallback
{
public void callback(String arg);
}
*)

let output_schema_binder index schema =
  let name = default_name "output" index in
  output "template <class T>";
  start_class name;

  output "public static void of_stmt(statement stmt, T& obj)";
  G.open_curly ();
  List.iteri (fun index attr -> get_column attr index) schema;
  G.close_curly "";

  end_class name;
  name 

let output_schema_binder index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder index schema)

let params_to_values = List.mapi (fun i (n,t) -> param_name_to_string n i, t >> param_type_to_string >> as_cxx_type)
let params_to_values = List.unique & params_to_values
let make_const_values = List.map (fun (name,t) -> name, sprintf "%s const&" t)

let output_value_defs vals =
  vals >> List.iter (fun (name,t) -> output "%s %s;" t name)

let schema_to_values = List.mapi (fun i attr -> name_of attr i, attr.RA.domain >> Type.to_string >> as_cxx_type)

let output_schema_data index schema =
  let name = default_name "data" index in
  start_class name;
  schema >> schema_to_values >> output_value_defs;
  end_class name

let output_value_inits vals =
  match vals with
  | [] -> ()
  | _ ->
    output " : %s"
    (String.concat "," (List.map (fun (name,_) -> sprintf "%s(%s)" name name) vals))

let output_params_binder index params =
  let name = default_name "params" index in
  start_class name;
  let values = params_to_values params in
  values >> make_const_values >> output_value_defs;
  empty_line ();
  output "%s(%s)" name (values >> make_const_values >> G.Values.to_string);
  output_value_inits values;
  G.open_curly ();
  G.close_curly "";
  empty_line ();
  output "void set_params(typename Traits::statement stmt)";
  G.open_curly ();
  List.iteri set_param params;
  G.close_curly "";
  empty_line ();
  end_class name;
  name

let output_params_binder index params =
  match params with
  | [] -> "typename Traits::no_params"
  | _ -> output_params_binder index params

type t = unit

let start () = ()

let generate_code () index schema params kind props =
   let schema_binder_name = output_schema_binder index schema in
   let params_binder_name = output_params_binder index params in
(*    if (Option.is_some schema_binder_name) then output_schema_data index schema; *)
(*    out_public (); *)
(*    if (Option.is_some schema_binder_name) then output "template<class T>"; *)
   let values = params_to_values params in
   let result = match schema_binder_name with None -> [] | Some _ -> ["result","T&"] in
   let all_params = G.Values.to_string
     (["db","typename Traits::connection"] @ result @ (make_const_values values)) 
   in
   let name = choose_name props kind index in
   let sql = G.quote (get_sql props kind params) in
   let inline_params = G.Values.inline (make_const_values values) in
   output "static bool %s(%s)" name all_params;
   G.open_curly ();
   begin match schema_binder_name with
   | None -> output "return Traits::do_execute(db,_T(%s),%s(%s));" sql params_binder_name inline_params
   | Some schema_name ->output "return Traits::do_select(db,result,_T(%s),%s(),%s(%s));" 
          sql (schema_name ^ "<typename T::value_type>") params_binder_name inline_params
   end;
   G.close_curly "";
   empty_line ()

let start_output () = start_class "sqlgg"
let finish_output () = end_class "sqlgg"

