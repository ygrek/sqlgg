(* C# code generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

module G = Gen_cxx
module J = Gen_java

let comment = G.comment
let empty_line = G.empty_line

let (start_class,end_class) = J.start_class,J.end_class
let (start_ns,end_ns) = J.start_ "namespace"

let as_db_type = function
  | Type.Int -> "Int32"
  | Type.Text -> "String"
  | Type.Float -> "Float"
  | Type.Blob -> "Blob"
  | Type.Bool -> "Boolean"
  | Type.Datetime -> "Datetime"

let as_cs_type = as_db_type

let get_column attr index =
  sprintf "(%s)reader[\"%s\"]"
    (attr.RA.domain >> as_cs_type)
    (attr.RA.name)

let param_type_to_string t = t >> Option.default Type.Text >> as_db_type

let schema_to_values = List.mapi (fun i attr -> name_of attr i, attr.RA.domain >> as_cs_type)

let output_schema_binder index schema =
  let name = "callback" in
  output "public delegate void %s(%s);" name (G.Values.to_string (schema_to_values schema));
  empty_line ();
  name

let output_schema_binder index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder index schema)

let params_to_values = List.mapi (fun i (n,t) -> param_name_to_string n i, t >> param_type_to_string)
let params_to_values = List.unique & params_to_values

let set_param index param =
  let (id,t) = param in
  let name = default_name "param" index in
  comment () "FIXME unnamed params";
  output "IDbDataParameter %s = cmd.CreateParameter();" name;
  output "%s.ParameterName = \"@%s\";" name (param_name_to_string id index);
  output "%s.DbType = DbType.%s;" name (param_type_to_string t);
  output "cmd.Parameters.Add(%s);" name

let output_params_binder params =
  List.iteri set_param params;
  output "cmd.Prepare();"

type t = unit

let start () = ()

let generate_code () index schema params kind props =
   let values = params_to_values params in
   let name = choose_name props kind index in
   let sql = G.quote (get_sql props kind params) in
   start_class name;
    output "IDbCommand cmd;";
    output "static string sql = %s;" sql;
    empty_line ();
    G.func "public" name ["db","IDbConnection"] (fun () ->
      output "cmd = db.CreateCommand();";
      output "cmd.CommandText = sql;";
      output_params_binder params;
    );
    empty_line ();
    let schema_binder_name = output_schema_binder index schema in
    let result = match schema_binder_name with None -> [] | Some name -> ["result",name] in
    let all_params = values @ result in
    G.func "public int" "execute" all_params (fun () ->
      List.iteri
        (fun i (name,_) -> output "((IDbDataParameter)cmd.Parameters[%u]).Value = %s;" i name)
        values;
      begin match schema_binder_name with
      | None -> output "return cmd.ExecuteNonQuery();"
      | Some name ->
         output "IDataReader reader = cmd.ExecuteReader();";
         let args = List.mapi (fun index attr -> get_column attr index) schema in
         let args = String.concat "," args in
         output "int count = 0;";
         output "while (reader.Read())";
         G.open_curly ();
         output "result(%s);" args;
         output "count++;";
         G.close_curly "";
         output "reader.Close();";
         output "return count;"
      end);
   end_class name

let start_output () name =
  output "using System;";
  output "using System.Data;";
  empty_line ();
  start_ns name

let finish_output () name = end_ns name

