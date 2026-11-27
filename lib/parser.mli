type parse_result = private {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
  relation_slots : Relation_slot.Name.t Sql.located list; [@default []] [@drop_default_if_json_equal]
  (** named relation slots, in source order *)
}
[@@deriving json, jsonschema]

val parse_stmt : string -> parse_result
