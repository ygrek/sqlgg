type line_directives =
  | Omit_line_directives
  | Emit_line_directives

type destination =
  | Stdout
  | File of { filename : string; line_directives : line_directives }

type migration_log = {
  source : string;
  output : string option;
}

type generation = private {
  dialect : [ `MySQL | `TiDB ];
  schema_file : string;
  initial_file : string option;
  baseline : Sqlgg_migrations.Migration_id.t option;
  migration_log : migration_log;
  manual_file : string option;
  module_name : string;
  now : int64 option;
  new_tables : Schema_diff.new_tables;
  destination : destination;
}

val parse : string array -> [ `Generate of generation | `Exit of int ]
