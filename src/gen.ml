(* Code generation *)

open Printf
open ExtLib
open Sqlgg
open Prelude
open Stmt

type subst_mode = | Named | Unnamed | Oracle | PostgreSQL

type stmt = { schema : Sql.schema_column list; vars : Sql.var list; kind : kind; props : Props.t; }

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
let output fmt = ksprintf indent_endline fmt
let output_l = List.iter indent_endline
let print fmt = ksprintf print_endline fmt
let indented k = inc_indent (); k (); dec_indent ()

let name_of attr index =
  match attr.Sql.name with
  | "" -> sprintf "_%u" index
  | s -> s

let make_param_name index (p:Sql.param_id) =
  match p.value with
  | None -> sprintf "_%u" index
  | Some s -> s

let show_param_name (p: Sql.Type.t Sql.param) index = make_param_name index p.id

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
  | Dynamic of Sql.param_id * sql_dynamic_ctor list 
  | SubstIn of Sql.Type.t Sql.param * Sql.Meta.t
  | DynamicIn of Sql.param_id * [`In | `NotIn] * sql list
  | SubstTuple of Sql.param_id * Sql.tuple_list_kind

and sql_dynamic_ctor = { 
  ctor: Sql.param_id; 
  args: Sql.var list option; 
  sql: sql list;
  is_poly: bool;
}

let substitute_vars s vars subst_param =
  let rec loop s acc i parami vars =
    match vars with
    | [] -> acc, i
    | Sql.Single (param, _) :: tl ->
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
      loop s acc i2 parami tl
    | SingleIn (param,m ):: tl ->
      let (i1,i2) = param.id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = SubstIn (param, m) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop s acc i2 parami tl
    | ChoiceIn { param = name; kind; vars } :: tl ->
      let (i1,i2) = name.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc =
        DynamicIn (name, kind, List.rev @@ fst @@ loop s [] i1 0 vars) ::
        Static (String.slice ~first:i ~last:i1 s) ::
        acc
      in
      loop s acc i2 parami tl
    | Choice (name,ctors) :: tl ->
      let dyn = process_ctors ~is_poly:true s i ctors in
      let (i1,i2) = name.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = Dynamic (name, dyn) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop s acc i2 parami tl
    | TupleList (id, Where_in { value = (types, in_not_in); pos = (j1, j2) }) :: tl ->
      let (i1,i2) = id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      assert (j2 > j1);
      assert (j1 > i); 
      let sub = [Static (String.slice ~first:j1 ~last:i1 s); SubstTuple (id, Where_in { value = (types, in_not_in); pos = (j1, j2) })] in
      let acc = DynamicIn (id, in_not_in, sub) :: Static (String.slice ~first:i ~last:j1 s) :: acc in
      loop s acc i2 parami tl
    | TupleList (id, ValueRows x) :: tl ->
      let (i1,i2) = id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = SubstTuple (id, ValueRows x) :: Static (String.slice ~first:i ~last:x.values_start_pos s) :: acc in
      loop s acc i2 parami tl
    | TupleList (id, kind) :: tl ->
      let (i1,i2) = id.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = SubstTuple (id, kind) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop s acc i2 parami tl
    (* Resuse Dynamic to avoid of making a new substitution constructor. *)  
    | OptionActionChoice (name, vars, ((f1, f2), (c1, c2)), kind) :: tl ->
      assert ((c2 = 0 && c1 = 1) || c2 > c1);
      assert (c1 > i);
      let pieces =
        let (acc, last) = loop s [] c1 0 vars in
        let s_choice = 
          let sql = [Static " ( "] @ List.rev(Static (String.slice ~first:last ~last:c2 s) :: acc)  @ [Static " ) "]in
          let ctor = Sql.{ value=Some("Some"); pos=(0, 0); } in
          let args = Some(vars) in
          {ctor; args; sql; is_poly=false} in
        let n = 
          let sql = Static (match kind with | BoolChoices -> " TRUE " | SetDefault -> " DEFAULT ") in
          let ctor = Sql.{ value=Some("None"); pos=(0, 0); } in
          let args = None in
          {ctor; args; sql=[sql]; is_poly=false} in
        [s_choice; n]
      in
      let acc = Dynamic (name, pieces) :: Static (String.slice ~first:i ~last:f1 s) :: acc in
       loop s acc f2 parami tl
    | SharedVarsGroup (shared_vars, id) :: tl ->
        let (i1,i2) = id.pos in
        assert (i2 > i1);
        assert (i1 > i);
        let shared_sql, (_: Sql.select_full) = Shared_queries.get id.value in
        let raw_processed = loop_and_squash shared_sql shared_vars in
        let processed_shared = [Static "("] @ raw_processed @ [Static ")"] in
        loop s (List.rev processed_shared @ Static (String.slice ~first:i ~last:i1 s) :: acc) i2 parami tl
    | DynamicSelect (name,ctors) :: tl ->
      let dyn = ctors |> List.map begin function
        | Sql.Simple (ctor, args) ->
          let (c1, c2) = ctor.pos in
          let sql = match args with
            | None | Some [] -> [Static (String.slice ~first:c1 ~last:c2 s)]
            | Some l ->
              let (acc, last) = loop s [] (c1 - 1) 0 l in
              let pieces = List.rev (Static (String.slice ~first:last ~last:c2 s) :: acc) in
              begin match pieces with
              | Static hd :: rest -> Static (String.slice ~first:1 hd) :: rest
              | _ -> pieces
              end
          in
          { ctor; sql; args; is_poly = false }
        | Verbatim (n, v) ->
          { ctor = { value = Some n; pos = (0,0) }; args = Some []; sql = [Static v]; is_poly = false }
      end in
      let (i1,i2) = name.pos in
      assert (i2 > i1);
      assert (i1 > i);
      let acc = Dynamic (name, dyn) :: Static (String.slice ~first:i ~last:i1 s) :: acc in
      loop s acc i2 parami tl
  and process_ctors ~is_poly s i ctors =
    ctors |> List.map begin function
      | Sql.Simple (ctor, args) ->
        let (c1, c2) = ctor.pos in
        assert ((c2 = 0 && c1 = 1) || c2 > c1);
        assert (c1 > i);
        let sql =
          match args with
          | None -> [Static ""]
          | Some l ->
            let (acc, last) = loop s [] c1 0 l in
            List.rev (Static (String.slice ~first:last ~last:c2 s) :: acc)
        in
        { ctor; sql; args; is_poly }
      | Verbatim (n, v) ->
        { ctor = { value = Some n; pos = (0,0) }; args = Some []; sql = [Static v]; is_poly }
    end
  and loop_and_squash sql vars =
    let acc, last = loop sql [] 0 0 vars in
    let acc = List.rev (Static (String.slice ~first:last sql) :: acc) in
    squash [] acc
  and squash acc = function
    | [] -> List.rev acc
    | Static s1 :: Static s2 :: tl -> squash acc (Static (s1 ^ s2) :: tl)
    | x::xs -> squash (x::acc) xs
  in
  loop_and_squash s vars


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
  param.typ.Type.nullability = Nullable

let is_attr_nullable attr =
  let open Sql in
  attr.domain.nullability = Nullable

type value = { vname : string; vtyp : string; nullable : bool; }

module Translate(T : LangTypes) = struct

let show_param_type p = T.as_api_type p.Sql.typ
let schema_to_values list = List.mapi (fun i attr -> { vname = name_of attr i; vtyp = T.as_lang_type attr.Sql.domain; nullable = is_attr_nullable attr || attr.domain.nullability = Nullable }) list
(* let schema_to_string = G.Values.to_string $ schema_to_values  *)
let all_params_to_values l =
  l |> List.mapi (fun i p -> { vname = show_param_name p i; vtyp = T.as_lang_type p.typ; nullable = is_param_nullable p || p.typ.Sql.Type.nullability = Nullable; })
  |> List.unique ~cmp:(fun v1 v2 -> String.equal v1.vname v2.vname)
(* rev unique rev -- to preserve ordering with respect to first occurrences *)
let values_of_params = List.rev $ List.unique ~cmp:(=) $ List.rev $ all_params_to_values

let rec find_param_ids l =
  List.concat @@
  List.map
    (function
      | Sql.Single (p, _) | SingleIn (p, _) -> [ p.id ]
      | Choice (id,_) -> [ id ]
      | OptionActionChoice (id, _, _, _) -> [id]
      | ChoiceIn { param; vars; _ } -> find_param_ids vars @ [param]
      | SharedVarsGroup (vars, _) -> find_param_ids vars
      | TupleList (id, _) -> [ id ]
      | DynamicSelect (id, _) -> [ id ])
    l

let names_of_vars l =
  find_param_ids l |>
  List.mapi make_param_name |>
  List.unique ~cmp:String.equal

let rec params_only l =
  List.concat @@
  List.map
    (function
      | Sql.Single (p, _) -> [p]
      | SingleIn _ -> []
      | SharedVarsGroup (vars, _)
      | ChoiceIn { vars; _ } -> params_only vars
      | OptionActionChoice _
      | Choice _ -> fail "dynamic choices not supported for this host language"
      | TupleList _ -> []
      | DynamicSelect _ -> fail "dynamic selects not supported for this host language (params_only)")
    l

let rec inparams_only l =
  List.concat @@
  List.map
    (function
      | Sql.SingleIn (p, _) -> [p]
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
