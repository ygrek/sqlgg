```text
 Code generation:
  -gen cxx|caml|caml_io|java|xml|csharp|sql|none   Set output language (default: none)
  -name <identifier>                               Set output module name (default: sqlgg)
  -params named|unnamed|oracle|postgresql|none     Output query parameters substitution (default: auto-detected from dialect, can be overridden)
  -category {all|none|[-]<category>{,<category>}+} Only generate code for these specific query categories (possible values: DDL DQL DML DCL TCL)
  -dynamic-select                                  Generate static and dynamic version for every SELECT (dynamic allows to pick columns per call)
  -                                                Read sql from stdin

 Schema and migrations:
  -diff                                            Generate up/down migrations from the diff of -base schema against -target schema
  -migrate                                         All-in-one: diff initial+migrations against target, append the up/down delta to the migrations file and (re)generate code
  -base <file>                                     Baseline schema / prior up migrations (repeatable)
  -target <file>                                   Target schema (repeatable)
  -initial <file>                                  Baseline DDL for -migrate (repeatable)
  -migrations-file <file>                          Generated migrations SQL for -migrate (read and appended in place; required only when there is a new delta to record)
  -extends <file>                                  Hand-written migrations SQL, merged with generated ones by `id` (read-only)
  -now <YYYYMMDDHHMMSS>                            Pin the migration id timestamp (default: current clock); ids are <timestamp>_<descriptive_name>
  -max-migration-id-length <N>                     Limit generated migration ids to N characters (default: no limit)
  -ddl-as-migration                                Write new tables as CREATE TABLE migrations instead of plain schema DDL

 Dialect and checks:
  -dialect mysql|postgresql|sqlite|tidb            Set SQL dialect. Queries can only use its features
  -no-check {all|<feature>{,<feature>}+}           Disable dialect feature checks (possible features: collation|join_on_subquery|create_table_as_select|on_duplicate_key|on_conflict|straight_join|lock_in_share_mode|fulltext_index|unsigned_types|autoincrement|replace_into|row_locking|default_expr|ttl|alter_column|user_defined_type)
  -allow-write-notnull-null                        Accept writing a nullable value into a NOT NULL column, instead of failing (MySQL, TiDB and SQLite only)

 Generated header:
  -no-header                                       Do not put version header in generated output
  -no-header-timestamp                             Do not put timestamp in version header in generated output
  -static-header                                   Only output short static header without version/timestamp

 Diagnostics:
  -show-tables                                     Show all current tables
  -show-table <name>                               Show specified table
  -debug <N>                                       Set debug level
  -version                                         Show version
  -test                                            Run unit tests
```
