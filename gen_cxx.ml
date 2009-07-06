(* C++ code generation *)

open ExtList
open ExtString
open Operators
open Printf
open Apply

open Stmt
open Gen
open Sql

module Values = struct

let names = List.map fst
let join = String.concat ", "
let inline = join & names
let to_string = join & (List.map (fun (n,t) -> t ^ " " ^ n))

end

let rec freshname name scope =
  match List.find_all ((=) name) scope with
  | [] -> name
  | _ -> freshname (name ^ "_") scope

let quote = String.replace_chars (function '\n' -> "\\n\\\n" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let quote_comment_inline = String.replace_chars (function '\n' -> "\n// " | c -> String.make 1 c)
let comment () fmt = Printf.kprintf (indent_endline & quote_comment_inline & (^) "// ") fmt
let empty_line () = print_newline ()
let open_curly () = output "{"; inc_indent ()
let close_curly fmt = dec_indent (); indent "}"; print fmt
let start_struct name =
   output "struct %s" name;
   open_curly ()
let end_struct name =
   close_curly "; // struct %s" name;
   empty_line ()
let out_public () = dec_indent(); output "public:"; inc_indent()
let out_private () = dec_indent(); output "private:"; inc_indent()

module L = struct
  let as_api_type = Type.to_string
  let as_lang_type t = "typename Traits::" ^ (as_api_type t)
end

module T = Translate(L)

open L
open T

let column_action action attr index =
  let typ = as_api_type attr.RA.domain in
  let name = name_of attr index in
  output "Traits::%s_column_%s(row, %u, %s);" action typ index name

let func head name args ?tail k =
  let tail = match tail with
  | Some s -> " " ^ s
  | None -> ""
  in
  let head = match head with "" -> "" | s -> s ^ " " in
  output "%s%s(%s)%s" head name (Values.to_string args) tail;
  open_curly ();
  k ();
  close_curly ""

let set_param arg index param =
  let (id,t) = param in
  output "Traits::set_param(%s, %s, %u);"
(*     (param_type_to_string t) *)
    arg
    (param_name_to_string id index)
    index

let output_value_defs vals =
  vals >> List.iter (fun (name,t) -> output "%s %s;" t name)

let output_schema_binder index schema =
  out_private ();
  let name = "output" in
  output "template <class T>";
  start_struct name;

  let vals = schema_to_values schema in
  output_value_defs vals;
  empty_line ();

  output "enum { count = %u };" (List.length schema);
  empty_line ();

  let mthd action active =
    func "void" action 
      (["row","typename Traits::row"] @ (if active then ["result","T"] else []))
      (fun () ->
        List.iteri (fun index attr -> column_action action attr index) schema;
        if active then output "result(%s);" (Values.inline vals)
      )
  in

  mthd "get" true;
  mthd "bind" false;

  end_struct name;
  name

let output_schema_binder index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder index schema)

let make_const_values = List.map (fun (name,t) -> name, sprintf "%s const&" t)

(*
let output_schema_data index schema =
  out_public ();
  let name = "data" in
  start_struct name;
  schema >> schema_to_values >> output_value_defs;
  end_struct name
*)

let value_inits vals =
  match vals with
  | [] -> ""
  | _ -> sprintf ": %s" (String.concat "," (List.map (fun (name,_) -> sprintf "%s(%s)" name name) vals))

let struct_params name values k =
  start_struct name;
  output_value_defs values;
  empty_line ();
  k ();
  end_struct name

let struct_ctor name values k =
  struct_params name values (fun () ->
    func "" name values ~tail:(value_inits values) Apply.id;
    empty_line ();
    k ())

let output_params_binder index params =
  out_private ();
  let name = "params" in
  let values = params_to_values params in
  struct_ctor name (make_const_values values) (fun () ->
    comment () "binding slots in a query (one param may be bound several times)";
    output "enum { count = %u };" (List.length params);
    empty_line ();
    let arg = freshname "target" (name :: Values.names values) in
    output "template <class T>";
    func "void" "set_params" [arg,"T&"] (fun () -> List.iteri (set_param arg) params);
    empty_line ()
  );
  name

let output_params_binder index params =
  match params with
  | [] -> "typename Traits::no_params"
  | _ -> output_params_binder index params

type t = unit

let start () = ()

let make_stmt index stmt =
   let name = choose_name stmt.props stmt.kind index in
   let sql = quote (get_sql stmt) in
   struct_params name ["stmt","typename Traits::statement"] (fun () ->
    func "" name ["db","typename Traits::connection"] ~tail:(sprintf ": stmt(db,SQLGG_STR(%s))" sql) Apply.id;
   let schema_binder_name = output_schema_binder index stmt.schema in
   let params_binder_name = output_params_binder index stmt.params in
(*    if (Option.is_some schema_binder_name) then output_schema_data index stmt.schema; *)
   out_public ();
   if (Option.is_some schema_binder_name) then output "template<class T>";
   let values = params_to_values stmt.params in
   let result = match schema_binder_name with None -> [] | Some _ -> ["result","T"] in
   let all_params = (make_const_values values) @ result in
   let inline_params = Values.inline (make_const_values values) in
   func "bool" "operator()" all_params (fun () ->
    begin match schema_binder_name with
    | None -> output "return stmt.execute(%s(%s));" params_binder_name inline_params
    | Some schema_name -> output "return stmt.select(result,%s<T>(),%s(%s));"
          schema_name params_binder_name inline_params
    end);
   );
   name

let make_all name names =
  List.iter (fun name -> output "%s %s;" name name) names;
  empty_line ();
  let tail = match names with
  | [] -> ""
  | _ -> ": " ^ (String.concat ", " (List.map (fun name -> sprintf "%s(db)" name) names))
  in
  func "" name ["db","typename Traits::connection"] ~tail Apply.id

let generate () name stmts =
  output "#pragma once";
  empty_line ();
  output "template <class Traits>";
  start_struct name;
  let stmts = List.of_enum stmts in
  let names = List.mapi make_stmt stmts in
  make_all name names;
  end_struct name

