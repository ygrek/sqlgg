type destination =
  | Stdout
  | File of string

type generation = private {
  dialect : [ `MySQL | `TiDB ];
  schema_file : string;
  initial_file : string option;
  baseline : Sqlgg_migrations.Migration_id.t option;
  migrations_file : string;
  manual_file : string option;
  module_name : Config_file.Migrations.Module_name.t;
  now : int64 option;
  new_tables : Schema_diff.new_tables;
  destination : destination;
}

val parse : string array -> [ `Generate of generation | `Exit of int ]
