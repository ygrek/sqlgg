(* Java code generation *)

open ExtLib
open Prelude
open Printf

open Stmt
open Gen
open Sql

module G = Gen_cxx

let comment = G.comment
let empty_line = G.empty_line

let quote = String.replace_chars (function '\n' -> "\" +\n\"" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let start_ cls =
   let f1 name =
   output "%s %s" cls name;
   G.open_curly ()
   in
   let f2 name =
   G.close_curly " // %s %s" cls name;
   empty_line ()
   in
   f1,f2

let (start_class,end_class) = start_ "public class"
let (start_intf,end_intf) = start_ "public static interface"

module L = struct

let as_lang_type = function
  | Type.Int -> "int"
  | Type.Text -> "String"
  | Type.Any -> "String"
  | Type.Float -> "float"
  | Type.Blob -> "Blob"
  | Type.Bool -> "boolean"
  | Type.Datetime -> "Timestamp"

let as_api_type = String.capitalize $ as_lang_type

end

module T = Translate(L)

open L
open T

let get_column attr index =
  sprintf "res.get%s(%u)"
    (attr.RA.domain |> as_api_type)
    (index + 1)

let output_schema_binder name _ schema =
  let name = sprintf "%s_callback" name in
  start_intf name;
  output "public void callback(%s);" (G.Values.to_string (schema_to_values schema));
  end_intf name;
  name

let output_schema_binder name index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder name index schema)

let output_value_defs vals =
  vals |> List.iter (fun (name,t) -> output "%s %s;" t name)

let output_schema_data index schema =
  let name = default_name "data" index in
  start_class name;
  schema |> schema_to_values |> output_value_defs;
  end_class name

let set_param name index param =
  let (id,t) = param in
  output "pstmt_%s.set%s(%u, %s);"
    name
    (t |> param_type_to_string |> String.capitalize)
    (index+1)
    (param_name_to_string id index)

let output_params_binder name _ params = List.iteri (set_param name) params

type t = unit

let start () = ()

let generate_code index stmt =
   let values = params_to_values stmt.params in
   let name = choose_name stmt.props stmt.kind index in
   let sql = quote (get_sql stmt) in
   output "PreparedStatement pstmt_%s;" name;
   empty_line ();
   let schema_binder_name = output_schema_binder name index stmt.schema in
   let result = match schema_binder_name with None -> [] | Some name -> ["result",name] in
   let all_params = values @ result in
   G.func "public int" name all_params ~tail:"throws SQLException" (fun () ->
      output "if (null == pstmt_%s)" name;
      output "  pstmt_%s = db.prepareStatement(%s);" name sql;
      output_params_binder name index stmt.params;
      begin match schema_binder_name with
      | None -> output "return pstmt_%s.executeUpdate();" name
      | Some _ ->
         output "ResultSet res = pstmt_%s.executeQuery();" name;
         let args = List.mapi (fun index attr -> get_column attr index) stmt.schema in
         let args = String.concat "," args in
         output "int count = 0;";
         output "while (res.next())";
         G.open_curly ();
         output "result.callback(%s);" args;
         output "count++;";
         G.close_curly "";
         output "return count;"
      end);
    empty_line ()

let generate () name stmts =
  params_mode := Some Unnamed; (* allow only unnamed params *)
  output "import java.sql.*;";
  empty_line ();
  start_class name;
  output "Connection db;";
  empty_line ();
  G.func "public" name ["aDb","Connection"] (fun () ->
    output "db = aDb;";
  );
  empty_line ();
  Enum.iteri generate_code stmts;
  end_class name

