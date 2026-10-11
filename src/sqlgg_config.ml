(**  *)
open Sqlgg

(** Sqlgg version *)
let version = "%%VERSION%%"

(** Debug level *)
let debug_level = ref 0

let set_debug_level n =
  debug_level := n;
  if n > 1 then Syntax.Config.debug := true

let debug1 () = !debug_level > 0

let gen_header : [ `Full | `Without_timestamp | `Static ] option ref = ref (Some `Full)

let include_category : [ `All | `None | `Only of Stmt.category list | `Except of Stmt.category list ] ref = ref `All

let dynamic_select = ref false

let no_check_features: Dialect.feature list ref = ref []

let set_no_check_features l = no_check_features := l

(* Options apply to the files that follow them, so the configuration is replaced as they are parsed. *)
let dialect = ref (Dialect.default_configuration Dialect.MySQL)

let requested_nonnullable_write = ref Sql_semantics.Nonnullable_write.Reject

let configure_dialect configuration =
  dialect :=
    match !requested_nonnullable_write with
    | Sql_semantics.Nonnullable_write.Replace_with_default -> Dialect.with_non_strict_writes configuration
    | Sql_semantics.Nonnullable_write.Reject -> configuration

let allow_write_notnull_null () =
  requested_nonnullable_write := Sql_semantics.Nonnullable_write.Replace_with_default;
  configure_dialect !dialect

let semantics () = Dialect.Semantic.of_configuration !dialect
