```text
 Code generation:
  -gen cxx|caml|caml_io|java|xml|json|csharp|sql|none Set output language (default: none)
  -name <identifier>                                  Set output module name (default: sqlgg)
  -params named|unnamed|oracle|postgresql|none        Output query parameters substitution (default: auto-detected from dialect, can be overridden)
  -category {all|none|[-]<category>{,<category>}+}    Only generate code for these specific query categories (possible values: DDL DQL DML DCL TCL)
  -open <file>                                        Make definitions (schema, reusable queries) from <file> available without generating code for it (unless the file is also given as an input)
  -dynamic-select                                     Generate static and dynamic version for every SELECT (dynamic allows to pick columns per call)
  -line-directives                                    Emit `# <line> "<file.sql>"` directives so that OCaml and merlin report errors in a generated query function on the line the query starts at (caml/caml_io only)
  -line-directives-file <file>                        Name of the generated file, used by -line-directives to hand numbering back to it outside of query functions (default: <input.sql>.ml for a single input file; required with several input files and with -migrate)
  -                                                   Read sql from stdin
  -json-schema                                        Print the JSON Schema of -gen json output and exit

 Schema and migrations:
  -diff                                               Generate up/down migrations from the diff of -base schema against -target schema
  -migrate                                            All-in-one: diff initial+migrations against target, append the up/down delta to the migrations file and (re)generate code
  -base <file>                                        Baseline schema / prior up migrations (repeatable)
  -target <file>                                      Target schema (repeatable)
  -initial <file>                                     Baseline DDL for -migrate (repeatable)
  -migrations-file <file>                             Generated migrations SQL for -migrate (read and appended in place unless -migrations-output is given; required only when there is a new delta to record)
  -migrations-output <file>                           Write the complete migration log here, leaving -migrations-file untouched
  -extends <file>                                     Hand-written migrations SQL, merged with generated ones by `id` (read-only)
  -now <YYYYMMDDHHMMSS>                               Pin the migration id timestamp (default: current clock); ids are <timestamp>_<descriptive_name>
  -max-migration-id-length <N>                        Limit generated migration ids to N bytes (default and maximum: 191)
  -ddl-as-migration                                   Write new tables as CREATE TABLE migrations instead of plain schema DDL
  -alter-algorithm default|instant|inplace|copy       Add ALGORITHM policy to generated ALTER TABLE statements (MySQL and TiDB only)
  -alter-lock default|none|shared|exclusive           Add LOCK policy to generated ALTER TABLE statements (MySQL only)

 Dialect and checks:
  -dialect mysql|postgresql|sqlite|tidb               Set SQL dialect. Queries can only use its features
  -no-check {all|<feature>{,<feature>}+}              Disable dialect feature checks (possible features: collation|join_on_subquery|create_table_as_select|on_duplicate_key|on_conflict|straight_join|lock_in_share_mode|fulltext_index|unsigned_types|autoincrement|replace_into|row_locking|default_expr|ttl|cached_table|alter_column|alter_algorithm|alter_lock|user_defined_type|extension|on_update_timestamp|column_comment|partition_operations|recover_table|rebase_auto_id|auto_id_cache|shard_row_id_bits|index_visibility|index_comment|table_comment|database_ddl|database_charset_and_collation|truncate_table|create_table_like|rename_table|tiflash_replica|sequence_ddl|enforced_check)
  -allow-write-notnull-null                           Accept writing a nullable value into a NOT NULL column, instead of failing (MySQL, TiDB and SQLite only)

 Generated header:
  -no-header                                          Do not put version header in generated output
  -no-header-timestamp                                Do not put timestamp in version header in generated output
  -static-header                                      Only output short static header without version/timestamp

 Diagnostics:
  -show-tables                                        Show all current tables
  -show-table <name>                                  Show specified table
  -debug <N>                                          Set debug level
  -version                                            Show version
  -test                                               Run unit tests
```
