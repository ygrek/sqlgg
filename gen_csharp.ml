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
module Values = G.Values

let comment = G.comment
let empty_line = G.empty_line

let comment_doc sl =
  output "/**";
  output_l (List.map (fun str -> Gen_caml.replace_all ~sub:"*/" ~by:"* /" ~str) sl);
  output "*/"

let comment_xml summary params =
  let summary = String.nsplit summary "\n" in
  let params = List.map (fun (n,s) -> sprintf "<param name=\"%s\">%s</param>" n s) params in
  comment_doc ("<summary>" :: summary @ ("</summary>" :: params))

let (start_class,end_class) = J.start_class,J.end_class
let (start_ns,end_ns) = J.start_ "namespace"

let quote = J.quote

let as_db_type = function
  | Type.Int -> "Int64"
  | Type.Text -> "String"
  | Type.Float -> "Float"
  | Type.Blob -> "String"
  | Type.Bool -> "Boolean"
  | Type.Datetime -> "Datetime"

let as_cs_type = as_db_type

let get_column attr index =
  sprintf "reader.Get%s(%u)"
    (attr.RA.domain >> as_db_type)
    index

let param_type_to_string t = t >> Option.default Type.Text >> as_db_type

let schema_to_values = List.mapi (fun i attr -> name_of attr i, attr.RA.domain >> as_cs_type)
let schema_to_string schema = schema >> schema_to_values >> G.Values.to_string

let output_schema_binder index schema =
  let name = "callback" in
  output "public delegate void %s(%s);" name (schema_to_string schema);
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
  output "IDbDataParameter %s = _cmd.CreateParameter();" name;
  output "%s.ParameterName = \"@%s\";" name (param_name_to_string id index);
  output "%s.DbType = DbType.%s;" name (param_type_to_string t);
  output "_cmd.Parameters.Add(%s);" name

let output_params_binder params =
  List.iteri set_param params;
  output "_cmd.Prepare();"

type t = unit

let start () = ()

let func_execute index stmt =
    let values = params_to_values stmt.params in
    let schema_binder_name = output_schema_binder index stmt.schema in
    let is_select = Option.is_some schema_binder_name in
    let doc = if is_select then ["result", schema_to_string stmt.schema] else [] in
    comment_xml "execute query" doc;
    let func_name = if is_select then "execute_reader" else "execute" in
    let result = "public " ^ if is_select then "IEnumerable<IDataReader>" else "int" in
    G.func result func_name values (fun () ->
      output "if (null == _cmd)";
      G.open_curly ();
      output "_cmd = _conn.CreateCommand();";
      output "_cmd.CommandText = sql;";
      output_params_binder stmt.params;
      G.close_curly "";
      output "if (null != CommandTimeout) _cmd.CommandTimeout = CommandTimeout.Value;";
      List.iteri
        (fun i (name,_) -> output "((IDbDataParameter)_cmd.Parameters[%u]).Value = %s;" i name)
        values;
      begin match schema_binder_name with
      | None -> output "return _cmd.ExecuteNonQuery();"
      | Some _ ->
         output "IDataReader reader = _cmd.ExecuteReader();";
         output "while (reader.Read())";
         G.open_curly ();
         output "yield return reader;";
         G.close_curly "";
         output "reader.Close();";
      end);
    if is_select then
    begin
      empty_line ();
      let result = match schema_binder_name with None -> [] | Some name -> ["result",name] in
      let all_params = values @ result in
      G.func "public int" "execute" all_params (fun () ->
         let args = List.mapi (fun index attr -> get_column attr index) stmt.schema in
         output "int count = 0;";
         output "foreach (var reader in execute_reader(%s))" (Values.inline values);
         G.open_curly ();
         output "result(%s);" (Values.join args);
         output "count++;";
         G.close_curly "";
         output "return count;"
      );
      empty_line ();
      match stmt.schema with
      | [attr] ->
        let t = as_cs_type attr.RA.domain in
        G.func ("public IEnumerable<" ^ t ^ ">") "rows" values (fun () ->
        output "foreach (var reader in execute_reader(%s))" (Values.inline values);
        G.open_curly ();
        output "yield return %s;" (get_column attr 0);
        G.close_curly ""
        )
      | _ ->
      start_class "row";
      List.iteri (fun index attr ->
        output "public %s %s { get { return %s; } }" 
          (as_cs_type attr.RA.domain)
          (name_of attr index)
          (get_column attr index)
      ) stmt.schema;
      output "private IDataReader reader;";
      empty_line ();
      G.func "public" "row" ["reader","IDataReader"] (fun () ->
        output "this.reader = reader;"
      );
      end_class "row";
      G.func "public IEnumerable<row>" "rows" values (fun () ->
        output "foreach (var reader in execute_reader(%s))" (Values.inline values);
        G.open_curly ();
        output "yield return new row(reader);";
        G.close_curly ""
      )
    end

let generate_code index stmt =
   let name = choose_name stmt.props stmt.kind index in
   let sql = quote (get_sql stmt) in
   start_class name;
    output "IDbCommand _cmd;";
    output "IDbConnection _conn;";
    output "public int? CommandTimeout;";
    output "static string sql = %s;" sql;
    empty_line ();
    G.func "public" name ["db","IDbConnection"] (fun () ->
      output "_cmd = null;";
      output "_conn = db;";
    );
    empty_line ();
    func_execute index stmt;
   end_class name;
   name

let generate_all names =
  start_class "all";
  List.iter (fun s -> output "public %s %s;" s s) names;
  empty_line ();
  G.func "public" "all" ["db","IDbConnection"] (fun () ->
    List.iter (fun name -> output "%s = new %s(db);" name name) names
  );
  end_class "all"

let generate () name stmts =
  params_mode := Some Named; (* only named params allowed *)
  let using = ["System";"System.Data";"System.Collections.Generic"] in
  List.iter (fun s -> output "using %s;" s) using;
  empty_line ();
  start_ns name;
  let names = List.mapi generate_code (List.of_enum stmts) in
  generate_all names;
  end_ns name

