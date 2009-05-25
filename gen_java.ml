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

let start_ cls =
   let f1 name =
   output "public %s %s" cls name;
   G.open_curly ()
   in
   let f2 name =
   G.close_curly " // class %s" name;
   empty_line ()
   in
   f1,f2

let (start_class,end_class) = start_ "class"
let (start_intf,end_intf) = start_ "interface"

let as_java_type = function
  | Type.Int -> "int"
  | Type.Text -> "String"
  | Type.Float -> "float"
  | Type.Blob -> "Blob"
  | Type.Bool -> "boolean"
  | Type.Datetime -> "Timestamp"

let get_column attr index =
  sprintf "res.get%s(%u)"
    (attr.RA.domain >> as_java_type >> String.capitalize)
    (index + 1)

let param_type_to_string t = t >> Option.default Type.Text >> as_java_type

let set_param index param =
  let (id,t) = param in
  output "pstmt.set%s(%u, %s);"
    (t >> param_type_to_string >> String.capitalize)
    index
    (param_name_to_string id index)

let schema_to_values = List.mapi (fun i attr -> name_of attr i, attr.RA.domain >> as_java_type)

let output_schema_binder index schema =
  let name = default_name "output" index in
  start_intf name;

  output "public void callback(%s);" (G.Values.to_string (schema_to_values schema));

  end_intf name;
  name

let output_schema_binder index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder index schema)

let params_to_values = List.mapi (fun i (n,t) -> param_name_to_string n i, t >> param_type_to_string)
let params_to_values = List.unique & params_to_values

let output_value_defs vals =
  vals >> List.iter (fun (name,t) -> output "%s %s;" t name)

let output_schema_data index schema =
  let name = default_name "data" index in
  start_class name;
  schema >> schema_to_values >> output_value_defs;
  end_class name

let output_params_binder index params =
(*
  let name = default_name "params" index in
  start_class name;
*)
(*
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
*)
  List.iteri set_param params;
  ()
(*
  G.close_curly "";
  empty_line ();
  end_class name;
  name
*)

type t = unit

let start () = ()

let generate_code () index schema params kind props =
   let schema_binder_name = output_schema_binder index schema in
   let values = params_to_values params in
   let result = match schema_binder_name with None -> [] | Some name -> ["result",name] in
   let all_params = G.Values.to_string
     (["db","Connection"] @ result @ values)
   in
   let name = choose_name props kind index in
   let sql = G.quote (get_sql props kind params) in
   output "public static int %s(%s) throws SQLException" name all_params;
   G.open_curly ();
   output "PreparedStatement pstmt = db.prepareStatement(%s);" sql;
   output_params_binder index params;
   begin match schema_binder_name with
   | None -> output "return pstmt.executeUpdate();"
   | Some name ->
      output "ResultSet res = pstmt.executeQuery();";
      let args = List.mapi (fun index attr -> get_column attr index) schema in
      let args = String.concat "," args in
      output "int count = 0;";
      output "while (res.next())";
      G.open_curly ();
      output "result.callback(%s);" args;
      output "count++;";
      G.close_curly "";
      output "return count;"
   end;
   G.close_curly "";
   empty_line ()

let start_output () =
  output "import java.sql.*;";
  empty_line ();
  start_class "sqlgg"

let finish_output () = end_class "sqlgg"

