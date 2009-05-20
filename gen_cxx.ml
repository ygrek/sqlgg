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

end

let quote = String.replace_chars (function '\n' -> "\\n\\\n" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let quote_comment_inline = String.replace_chars (function '\n' -> "\n// " | c -> String.make 1 c)
let comment () fmt = Printf.kprintf (indent_endline & quote_comment_inline & (^) "// ") fmt
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
let in_namespace name f =   
  output "namespace %s" name;
  open_curly ();
  let result = f () in
  close_curly " // namespace %s" name;
  empty_line ();
  result

let name_of attr index = 
  match attr.RA.name with
  | "" -> sprintf "_%u" index
  | s -> s

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

let output_scheme_binder index scheme =
  out_private ();
  let name = default_name "output" index in
  output "template <class T>";
  start_struct name;

  output "static void of_stmt(typename Traits::statement stmt, T& obj)";
  open_curly ();
  List.iteri (fun index attr -> get_column attr index) scheme;
  close_curly "";

  end_struct name;
  name 

let output_scheme_binder index scheme =
  match scheme with
  | [] -> None
  | _ -> Some (output_scheme_binder index scheme)

let params_to_values = List.mapi (fun i (n,t) -> param_name_to_string n i, t >> param_type_to_string >> as_cxx_type)
let params_to_values = List.unique & params_to_values
let make_const_values = List.map (fun (name,t) -> name, sprintf "%s const&" t)

let output_value_defs vals =
  vals >> List.iter (fun (name,t) -> output "%s %s;" t name)

let scheme_to_values = List.mapi (fun i attr -> name_of attr i, attr.RA.domain >> Type.to_string >> as_cxx_type)

let output_scheme_data index scheme =
  out_public ();
  let name = default_name "data" index in
  start_struct name;
  scheme >> scheme_to_values >> output_value_defs;
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
  output "void set_params(typename Traits::statement stmt)";
  open_curly ();
  List.iteri set_param params;
  close_curly "";
  empty_line ();
  end_struct name;
  name

let output_params_binder index params =
  match params with
  | [] -> "typename Traits::no_params"
  | _ -> output_params_binder index params

type t = unit

let start () = ()

let generate_code () index scheme params kind props =
   let scheme_binder_name = output_scheme_binder index scheme in
   let params_binder_name = output_params_binder index params in
   if (Option.is_some scheme_binder_name) then output_scheme_data index scheme;
   out_public ();
   if (Option.is_some scheme_binder_name) then output "template<class T>";
   let values = params_to_values params in
   let result = match scheme_binder_name with None -> [] | Some _ -> ["result","T&"] in
   let all_params = Values.to_string
     (["db","typename Traits::connection"] @ result @ (make_const_values values)) 
   in
   let name = choose_name props kind index in
   let sql = quote (get_sql props kind params) in
   let inline_params =Values.inline (make_const_values values) in
   output "static bool %s(%s)" name all_params;
   open_curly ();
   begin match scheme_binder_name with
   | None -> output "return Traits::do_execute(db,_T(%s),%s(%s));" sql params_binder_name inline_params
   | Some scheme_name ->output "return Traits::do_select(db,result,_T(%s),%s(),%s(%s));" 
          sql (scheme_name ^ "<typename T::value_type>") params_binder_name inline_params
   end;
   close_curly "";
   empty_line ()

let start_output () =
  output "#pragma once"; 
  empty_line ();
  output "template <class Traits>";
  start_struct "sqlgg"

let finish_output () = end_struct "sqlgg"

