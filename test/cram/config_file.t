`-config` takes the options from a JSON file instead of the command line.

  $ mkdir -p project/schema project/queries
  $ cat > project/schema/person.sql <<'EOF'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     name TEXT NOT NULL
  > );
  > EOF
  $ cat > project/queries/person.sql <<'EOF'
  > -- @find_person
  > SELECT * FROM person WHERE name = @name;
  > EOF
  $ cat > project/sqlgg.json <<'EOF'
  > {
  >   "dialect": "mysql",
  >   "header": "static",
  >   "gen": "caml_io",
  >   "name": "Make",
  >   "params": "unnamed",
  >   "schema": ["schema/*.sql"],
  >   "inputs": ["queries/person.sql"]
  > }
  > EOF

The output is what the same options give on the command line. File names in
the configuration are relative to it, so the command works from any directory:

  $ sqlgg -config project/sqlgg.json > from_config.ml
  $ sqlgg -dialect mysql -static-header -gen caml_io -name Make -params unnamed -open project/schema/person.sql project/queries/person.sql > from_flags.ml
  $ cmp from_config.ml from_flags.ml
  $ grep -q 'let find_person' from_config.ml && echo generated
  generated

`schema` is `-open`: its tables are known, and no code is generated for them.

  $ grep 'create_person' from_config.ml
  [1]

The configuration supplies defaults. Command-line options override them
regardless of where `-config` stands:

  $ sqlgg -config project/sqlgg.json -name Other | grep '^module'
  module Other (T : Sqlgg_traits.M_io) = struct
  $ sqlgg -name Other -config project/sqlgg.json | grep '^module'
  module Other (T : Sqlgg_traits.M_io) = struct
  $ sqlgg -config=project/sqlgg.json -name Other | grep '^module'
  module Other (T : Sqlgg_traits.M_io) = struct

Only one configuration file is accepted:

  $ sqlgg -config project/sqlgg.json -config project/shared.json
  -config may be given only once
  [2]

A configuration without `inputs` holds the shared options, and the files come
from the command line:

  $ cat > project/shared.json <<'EOF'
  > { "dialect": "mysql", "header": "none", "gen": "caml", "schema": ["schema/person.sql"] }
  > EOF
  $ sqlgg -config project/shared.json project/queries/person.sql | grep -q 'let find_person' && echo generated
  generated

`category` keeps only the listed kinds of statements:

  $ cat > project/ddl.json <<'EOF'
  > { "header": "none", "gen": "caml", "category": ["ddl"], "inputs": ["schema/person.sql", "queries/person.sql"] }
  > EOF
  $ sqlgg -config project/ddl.json | grep -o -E 'let (create_person|find_person)'
  let create_person

The legacy `-diff` and `-migrate` modes are the `mode` key. A mode carries its
own files and options, so a file cannot give `-diff` the files of `-migrate`.
`-now` stays on the command line:

  $ mkdir legacy
  $ cat > legacy/initial.sql <<'EOF'
  > CREATE TABLE person (id INT PRIMARY KEY);
  > EOF
  $ cat > legacy/target.sql <<'EOF'
  > CREATE TABLE person (id INT PRIMARY KEY, age INT);
  > EOF
  $ : > legacy/migrations.sql
  $ cat > legacy/sqlgg.json <<'EOF'
  > {
  >   "dialect": "mysql",
  >   "header": "none",
  >   "gen": "caml_io",
  >   "name": "Make",
  >   "mode": ["migrate", {
  >     "initial": ["initial.sql"],
  >     "migrations_file": "migrations.sql",
  >     "target": ["target.sql"]
  >   }]
  > }
  > EOF
  $ sqlgg -config legacy/sqlgg.json -now 20260101000000 > migrations.ml
  appended 1 migration(s) to legacy/migrations.sql (id 20260101000000_alter_person_add_col_age)
  $ cat legacy/migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_person_add_col_age
  ALTER TABLE `person` ADD COLUMN `age` INT;
  ALTER TABLE `person` DROP COLUMN `age`;
  $ sqlgg -no-header -dialect mysql -migrate -gen caml_io -name Make -initial legacy/initial.sql -migrations-file legacy/migrations.sql -target legacy/target.sql | cmp migrations.ml -
  nothing new to migrate; regenerated code from 1 recorded migration(s)

  $ cat > legacy/diff.json <<'EOF'
  > {
  >   "dialect": "mysql",
  >   "header": "none",
  >   "gen": "sql",
  >   "mode": ["diff", { "base": ["initial.sql"], "target": ["target.sql"] }]
  > }
  > EOF
  $ sqlgg -config legacy/diff.json -now 20260101000000 > diff_from_config.sql
  $ sqlgg -no-header -dialect mysql -gen sql -diff -base legacy/initial.sql -target legacy/target.sql -now 20260101000000 | cmp diff_from_config.sql -
  $ grep -c 'ADD COLUMN `age`' diff_from_config.sql
  1
  $ cat > legacy/mixed.json <<'EOF'
  > { "mode": ["diff", { "base": ["initial.sql"], "initial": ["initial.sql"] }] }
  > EOF
  $ sqlgg -config legacy/mixed.json
  legacy/mixed.json: did not expect field "initial" but got ["diff", {"base": ["initial.sql"], "initial": ["initial.sql"]}]
  [2]

`schema` is `-open`, which these modes do not take:

  $ cat > legacy/open.json <<'EOF'
  > { "schema": ["initial.sql"], "mode": ["diff", { "base": ["initial.sql"], "target": ["target.sql"] }] }
  > EOF
  $ sqlgg -config legacy/open.json
  legacy/open.json: schema is not supported together with mode
  [2]
  $ cat > legacy/inputs.json <<'EOF'
  > { "inputs": ["initial.sql"], "mode": ["diff", { "base": ["initial.sql"] }] }
  > EOF
  $ sqlgg -config legacy/inputs.json
  legacy/inputs.json: inputs are not supported together with mode
  [2]
  $ sqlgg -config legacy/diff.json legacy/initial.sql
  input files are not supported with -diff/-migrate
  [1]

A key that sqlgg does not know is an error, not a silently ignored typo:

  $ cat > typo.json <<'EOF'
  > { "dialekt": "mysql" }
  > EOF
  $ sqlgg -config typo.json
  typo.json: did not expect field "dialekt" but got {"dialekt": _}
  [2]

So is a value outside the ones the matching option takes:

  $ cat > bad_gen.json <<'EOF'
  > { "gen": "rust" }
  > EOF
  $ sqlgg -config bad_gen.json
  bad_gen.json: expected "cxx" or "caml" or "caml_io" or "java" or "xml" or "json" or "csharp" or "sql" or "none" but got "rust"
  [2]
  $ cat > bad_dialect.json <<'EOF'
  > { "dialect": "oracle" }
  > EOF
  $ sqlgg -config bad_dialect.json
  bad_dialect.json: unknown dialect "oracle" (expected mysql|postgresql|sqlite|tidb)
  [2]
  $ cat > bad_category.json <<'EOF'
  > { "category": ["ddl", "everything"] }
  > EOF
  $ sqlgg -config bad_category.json
  bad_category.json: unknown category "everything" (expected ddl|dql|dml|dcl|tcl)
  [2]
  $ cat > bad_params.json <<'EOF'
  > { "params": "positional" }
  > EOF
  $ sqlgg -config bad_params.json
  bad_params.json: unknown params mode "positional" (expected auto|none|named|unnamed|oracle|postgresql)
  [2]

A `schema` pattern that matches nothing is reported, since the queries would
fail later with a less direct message:

  $ mkdir project/tables
  $ cat > project/no_schema.json <<'EOF'
  > { "schema": ["tables/*.sql"] }
  > EOF
  $ sqlgg -config project/no_schema.json
  project/no_schema.json: schema "tables/*.sql" matches no files
  [2]
  $ cat > project/bad_glob.json <<'EOF'
  > { "schema": ["tables/["] }
  > EOF
  $ sqlgg -config project/bad_glob.json
  project/bad_glob.json: invalid schema glob "tables/["
  [2]

Text that is not JSON and a missing file:

  $ echo '{ "gen": ' > broken.json
  $ sqlgg -config broken.json
  broken.json: Line 2, bytes -1-0:
  Unexpected end of input
  [2]
  $ sqlgg -config absent.json
  cannot read absent.json: absent.json: No such file or directory
  [2]

`-config-json-schema` describes the file for editors and validators:

  $ sqlgg -config-json-schema > schema.json
  $ grep -c '"additionalProperties": false' schema.json
  4
  $ grep -o -E '"(dialect|gen|schema|inputs|migrations)": \{' schema.json | sort -u
  "dialect": {
  "gen": {
  "inputs": {
  "migrations": {
  "schema": {

It lists the values of the keys that take one, spelled as on the command line:

  $ tr -d ' \n' < schema.json | grep -o '"enum":\["mysql"[^]]*\]'
  "enum":["mysql","postgresql","sqlite","tidb"]
