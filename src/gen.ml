(* Code generation *)

open Printf
open ExtLib
open Sqlgg
open Prelude
open Stmt

type subst_mode = | Named | Unnamed | Oracle | PostgreSQL

type stmt = { schema : Sql.Schema.t; vars : Sql.var list; kind : kind; props : Props.t; }

(** defines substitution function for parameter literals *)
let params_mode = ref None

let (inc_indent,dec_indent,make_indent) =
  let v = ref 0 in
  (fun () -> v := !v + 2),
  (fun () -> v := !v - 2),
  (fun () -> String.make !v ' ')

let print_indent () = print_string (make_indent ())
let indent s = print_indent (); print_string s
let indent_endline s = print_indent (); print_endline @@ String.trim s
let output fmt = kprintf indent_endline fmt
let output_l = List.iter indent_endline
let print fmt = kprintf print_endline fmt
let indented k = inc_indent (); k (); dec_indent ()

let name_of attr index =
  match attr.Sql.name with
  | "" -> sprintf "_%u" index
  | s -> s

let make_param_name index (p:Sql.param_id) =
  match p.label with
  | None -> sprintf "_%u" index
  | Some s -> s

let show_param_name (p:Sql.param) index = make_param_name index p.id

let make_name props default = Option.default default (Props.get props "name")
let default_name str index = sprintf "%s_%u" str index

let choose_name props kind index =
  let safename = String.map begin function
  | ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c) -> c
  | _ -> '_'
  end in
  let fix' s =
    match Props.get props "subst" with
    | Some x -> let (_,s) = String.replace ~str:s ~sub:("%%"^x^"%%") ~by:x in safename s
    | None -> safename s
  in
  let fix t = fix' @@ Sql.show_table_name t in
  let name = match kind with
  | Create t -> sprintf "create_%s" (fix t)
  | CreateIndex t -> sprintf "create_index_%s" (fix' t)
  | Update (Some t) -> sprintf "update_%s_%u" (fix t) index
  | Update None -> sprintf "update_%u" index
  | Insert (_,t) -> sprintf "insert_%s_%u" (fix t) index
  | Delete t -> sprintf "delete_%s_%u" (String.concat "_" @@ List.map fix t) index
  | Alter t -> sprintf "alter_%s_%u" (String.concat "_" @@ List.map fix t) index
  | Drop t -> sprintf "drop_%s" (fix t)
  | Select _  -> sprintf "select_%u" index
  | CreateRoutine s -> sprintf "create_routine_%s" (fix s)
  | Other -> sprintf "statement_%u" index
  in
  make_name props name

type sql =
    Static of string
  | Dynamic of (Sql.param_id * (Sql.param_id * Sql.var list option * sql list) list)
  | SubstIn of Sql.param
  | DynamicIn of Sql.param_id * [`In | `NotIn] * sql list
  | SubstTuple of Sql.param_id * Sql.schema

let substitute_vars s vars subst_param =
  let rec loop acc i parami vars =
    match vars with
    | [] -> acc, i
    | Sql.Single param :: tl ->
      let (i1,i2) = param.id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc, parami =
        match subst_param with
        | None -> Static (String.slice ~first:i ~last:i2 s) :: acc, parami
        | Some subst ->
          Static (subst parami param) ::
          Static (String.slice ~first:i ~last:i1 s) ::
          acc,
          parami + 1
      in
      loop acc i2 parami tl
    | SingleIn param :: tl ->
      let (i1,i2) = param.id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = SubstIn param :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop acc i2 parami tl
    | ChoiceIn { param = name; kind; vars } :: tl ->
      let (i1,i2) = name.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc =
        DynamicIn (name, kind, List.rev @@ fst @@ loop [] i1 0 vars) ::
        Static (String.slice ~first:i ~last:i1 s) ::
        acc
      in
      loop acc i2 parami tl
    | Choice (name,ctors) :: tl ->
      let dyn = ctors |> List.map begin function
        | Sql.Simple (ctor,args) ->
          let (c1,c2) = ctor.pos in
          assert ((c2 = 0 && c1 = 1) || c2 > c1);
          assert (c1 > i);
          let pieces =
            match args with
            | None -> [Static ""]
            | Some l ->
              let (acc,last) = loop [] c1 0 l in
              List.rev (Static (String.slice ~first:last ~last:c2 s) :: acc)
          in
          ctor, args, pieces
        | Verbatim (n,v) -> { label = Some n; pos = (0,0) }, Some [], [Static v]
        end
      in
      let (i1,i2) = name.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = Dynamic (name, dyn) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop acc i2 parami tl
    | TupleList (id, schema) :: tl ->
      let (i1,i2) = id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = SubstTuple (id, schema) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop acc i2 parami tl
  in
  let (acc,last) = loop [] 0 0 vars in
  let acc = List.rev (Static (String.slice ~first:last s) :: acc) in
  let rec squash acc = function
  | [] -> List.rev acc
  | Static s1 :: Static s2 :: tl -> squash acc (Static (s1 ^ s2) :: tl)
  | x::xs -> squash (x::acc) xs
  in
  squash [] acc

let subst_named index p = "@" ^ (show_param_name p index)
let subst_oracle index p = ":" ^ (show_param_name p index)
let subst_postgresql index _ = "$" ^ string_of_int (index + 1)
let subst_unnamed _ _ = "?"

let get_sql stmt =
  let sql = Props.get stmt.props "sql" |> Option.get in
  let subst =
    match !params_mode with
    | None -> None
    | Some subst ->
      Some (match subst with
      | Named -> subst_named
      | Unnamed -> subst_unnamed
      | Oracle -> subst_oracle
      | PostgreSQL -> subst_postgresql)
  in
  substitute_vars sql stmt.vars subst

let get_sql_string_only stmt =
  match get_sql stmt with
  | Static s :: [] -> s
  | _ -> fail "dynamic choices not supported for this language"

let time_string () =
  let module U = Unix in
  let t = U.time () |> U.gmtime in
  sprintf "%04u-%02u-%02uT%02u:%02uZ" (1900 + t.U.tm_year) (t.U.tm_mon+1) t.U.tm_mday t.U.tm_hour t.U.tm_min

module type LangTypes = sig

val as_api_type : Sql.Type.t -> string
val as_lang_type : Sql.Type.t -> string

end

let is_param_nullable param =
  let open Sql in
  match param.attr with None -> false | Some attr -> Constraints.mem Null attr.extra || Constraints.mem Autoincrement attr.extra

let is_attr_nullable attr =
  let open Sql in
  Constraints.mem Null attr.extra

type value = { vname : string; vtyp : string; nullable : bool; }

module Translate(T : LangTypes) = struct

let show_param_type p = T.as_api_type p.Sql.typ
let schema_to_values = List.mapi (fun i attr -> { vname = name_of attr i; vtyp = T.as_lang_type attr.Sql.domain; nullable = is_attr_nullable attr })
(* let schema_to_string = G.Values.to_string $ schema_to_values  *)
let all_params_to_values l =
  l |> List.mapi (fun i p -> { vname = show_param_name p i; vtyp = T.as_lang_type p.typ; nullable = is_param_nullable p; })
  |> List.unique ~cmp:(fun v1 v2 -> String.equal v1.vname v2.vname)
(* rev unique rev -- to preserve ordering with respect to first occurrences *)
let values_of_params = List.rev $ List.unique ~cmp:(=) $ List.rev $ all_params_to_values

let rec find_param_ids l =
  List.concat @@
  List.map
    (function
      | Sql.Single p | SingleIn p -> [ p.id ]
      | Choice (id,_) -> [ id ]
      | ChoiceIn { param; vars; _ } -> find_param_ids vars @ [param]
      | TupleList (id, _) -> [ id ])
    l

let names_of_vars l =
  find_param_ids l |>
  List.mapi make_param_name |>
  List.unique ~cmp:String.equal

let rec params_only l =
  List.concat @@
  List.map
    (function
      | Sql.Single p -> [p]
      | SingleIn _ -> []
      | ChoiceIn { vars; _ } -> params_only vars
      | Choice _ -> fail "dynamic choices not supported for this host language"
      | TupleList _ -> [])
    l

let rec inparams_only l =
  List.concat @@
  List.map
    (function
      | Sql.SingleIn p -> [p]
      | ChoiceIn { vars; _ } -> inparams_only vars
      | _ -> [])
    l

end

module type Generator = sig
  type t
  val generate : t -> string -> stmt list -> unit
  val start : unit -> t
  val comment : t -> ('a,unit,string,unit) format4 -> 'a
  val empty_line : t -> unit
end

module Make(S : Generator) = struct

let generate_header out mode =
  S.comment out "DO NOT EDIT MANUALLY";
  S.comment out "";
  let suffix =
    match mode with
    | `Full -> sprintf " %s on %s" Sqlgg_config.version (time_string ())
    | `Without_timestamp -> sprintf " %s" Sqlgg_config.version
    | `Static -> ""
  in
  S.comment out "generated by sqlgg%s" suffix;
  S.empty_line out

let process name stmts =
  let out = S.start () in
  Option.may (generate_header out) !Sqlgg_config.gen_header;
  S.generate out name stmts

end
