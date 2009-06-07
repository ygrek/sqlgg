(* C++ code generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

module Values = struct

let to_string x =
  String.concat ", " (List.map (fun (n,t) -> t ^ " " ^ n) x)

let inline x =
  String.concat ", " (List.map (fun (n,t) -> n) x)

let names = List.map fst

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

let name_of attr index =
  match attr.RA.name with
  | "" -> sprintf "_%u" index
  | s -> s

let column_action action attr index =
  output "Traits::%s_column_%s(row, %u, obj.%s);"
    action
    (Type.to_string attr.RA.domain)
    index
    (name_of attr index)

let func ret name args ?tail k =
  let tail = match tail with
  | Some s -> " " ^ s
  | None -> ""
  in
  output "%s %s(%s)%s" ret name (Values.to_string args) tail;
  open_curly ();
  k ();
  close_curly ""

(*
let get_column = column_action "get"
let bind_column = column_action "bind"
*)

let param_type_to_string t = Option.map_default Type.to_string "Any" t
let as_cxx_type str = "typename Traits::" ^ str

let set_param arg index param =
  let (id,t) = param in
  output "Traits::set_param(%s, %s, %u);"
(*     (param_type_to_string t) *)
    arg
    (param_name_to_string id index)
    index

let output_schema_binder index schema =
  out_private ();
  let name = default_name "output" index in
  output "template <class T>";
  start_struct name;

  output "enum { count = %u };" (List.length schema);
  empty_line ();

  let mthd action =
    func "static void" action ["row","typename Traits::row"; "obj","T&"] (fun () ->
      List.iteri (fun index attr -> column_action action attr index) schema)    
  in

  mthd "get";
  mthd "bind";

  end_struct name;
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
  out_public ();
  let name = default_name "data" index in
  start_struct name;
  schema >> schema_to_values >> output_value_defs;
  end_struct name

let output_value_inits vals =
  match vals with
  | [] -> ()
  | _ ->
    output " : %s"
    (String.concat "," (List.map (fun (name,_) -> sprintf "%s(%s)" name name) vals))

let output_params_binder index params =
  out_private ();
  let name = default_name "params" index in
  start_struct name;
  let values = params_to_values params in
  values >> make_const_values >> output_value_defs;
  empty_line ();
  output "%s(%s)" name (values >> make_const_values >> Values.to_string);
  output_value_inits values;
  open_curly ();
  close_curly "";
  empty_line ();
  comment () "binding slots in a query (one param may be bound several times)";
  output "enum { count = %u };" (List.length params);
  empty_line ();
  let arg = freshname "target" (name :: Values.names values) in
  output "template <class T>";
  func "void" "set_params" [arg,"T&"] (fun () -> List.iteri (set_param arg) params);
  empty_line ();
  end_struct name;
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
   if (Option.is_some schema_binder_name) then output_schema_data index schema;
   out_public ();
   if (Option.is_some schema_binder_name) then output "template<class T>";
   let values = params_to_values params in
   let result = match schema_binder_name with None -> [] | Some _ -> ["result","T&"] in
   let all_params = ["db","typename Traits::connection"] @ result @ (make_const_values values) in
   let name = choose_name props kind index in
   let sql = quote (get_sql props kind params) in
   let inline_params = Values.inline (make_const_values values) in
   func "static bool" name all_params (fun () ->
    begin match schema_binder_name with
    | None -> output "return Traits::do_execute(db,SQLGG_STR(%s),%s(%s));" sql params_binder_name inline_params
    | Some schema_name ->output "return Traits::do_select(db,result,SQLGG_STR(%s),%s(),%s(%s));"
          sql (schema_name ^ "<typename T::value_type>") params_binder_name inline_params
    end);
   empty_line ()

let start_output () name =
  output "#pragma once";
  empty_line ();
  output "template <class Traits>";
  start_struct name

let finish_output () name = end_struct name

