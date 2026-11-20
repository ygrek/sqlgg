`sqlgg migrations generate --config` reads its options from the `migrations`
object of a sqlgg configuration file, and the dialect from the top level.

  $ mkdir db
  $ cat > db/schema.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY
  > );
  > SQL
  $ cat > db/sqlgg.json <<'JSON'
  > {
  >   "dialect": "mysql",
  >   "migrations": {
  >     "lang": "ocaml",
  >     "schema": "schema.sql",
  >     "migrations": "migrations.sql",
  >     "module": "Migrations",
  >     "new_tables": "migrations",
  >     "output": "migrations.ml"
  >   }
  > }
  > JSON

File names are relative to the configuration, not to the working directory:

  $ sqlgg migrations generate --config db/sqlgg.json --now 20261112000000
  appended 1 migration(s) to db/migrations.sql (id 20261112000000_create_users)
  $ cat db/migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20261112000000_create_users
  CREATE TABLE `users` (`id` INT NOT NULL, PRIMARY KEY (`id`));
  DROP TABLE `users`;

The module is the one the same options give on the command line:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema db/schema.sql --migrations db/migrations.sql --module Migrations --ddl-as-migration > from_flags.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ cmp db/migrations.ml from_flags.ml

An option on the command line wins over the configuration:

  $ sqlgg migrations generate --config db/sqlgg.json --module Other --output other.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ tail -n 1 other.ml
  end (* module Other *)

The configuration may hold only a part of the options. The rest is still
required:

  $ cat > db/partial.json <<'JSON'
  > { "dialect": "mysql", "migrations": { "lang": "ocaml", "schema": "schema.sql" } }
  > JSON
  $ sqlgg migrations generate --config db/partial.json
  Usage: sqlgg migrations generate [--help] [OPTION]…
  sqlgg migrations: required option --migrations is missing
  [2]
  $ sqlgg migrations generate --config db/partial.json --migrations db/migrations.sql --module Migrations > partial.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ cmp db/migrations.ml partial.ml

The values of the configuration go through the same checks as the options:

  $ cat > db/bad_module.json <<'JSON'
  > { "dialect": "mysql", "migrations": { "lang": "ocaml", "schema": "schema.sql", "migrations": "migrations.sql", "module": "foo-bar" } }
  > JSON
  $ sqlgg migrations generate --config db/bad_module.json
  sqlgg migrations: db/bad_module.json: invalid OCaml module name "foo-bar"
  [2]
  $ cat > db/bad_baseline.json <<'JSON'
  > { "dialect": "mysql", "migrations": { "lang": "ocaml", "schema": "schema.sql", "migrations": "migrations.sql", "module": "Migrations", "baseline": "not an id" } }
  > JSON
  $ sqlgg migrations generate --config db/bad_baseline.json
  sqlgg migrations: db/bad_baseline.json: invalid migration id: not an id
  [2]
  $ cat > db/java.json <<'JSON'
  > { "dialect": "mysql", "migrations": { "lang": "java", "schema": "schema.sql", "migrations": "migrations.sql", "module": "Migrations" } }
  > JSON
  $ sqlgg migrations generate --config db/java.json
  sqlgg migrations: there is no migration runtime for --lang java yet
                    (supported: ocaml)
  [2]
  $ cat > db/typo.json <<'JSON'
  > { "dialect": "mysql", "migrations": { "langauge": "ocaml" } }
  > JSON
  $ sqlgg migrations generate --config db/typo.json
  sqlgg migrations: db/typo.json: did not expect field "langauge" but got
                    {"langauge": _}
  [2]
  $ sqlgg migrations generate --config db/absent.json
  sqlgg migrations: cannot read db/absent.json: db/absent.json: No such file or
                    directory
  [2]

A configuration shared with code generation keeps its other keys. The main
command line ignores `migrations`, and this command reads nothing but `dialect`
outside of it:

  $ cat > db/shared.json <<'JSON'
  > {
  >   "dialect": "mysql",
  >   "gen": "caml",
  >   "header": "none",
  >   "inputs": ["schema.sql"],
  >   "migrations": {
  >     "lang": "ocaml",
  >     "schema": "schema.sql",
  >     "migrations": "migrations.sql",
  >     "module": "Migrations"
  >   }
  > }
  > JSON
  $ sqlgg migrations generate --config db/shared.json > shared.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ sqlgg -config db/shared.json | grep -c 'let create_users'
  1

That covers the top-level `schema` too. Its patterns are for code generation,
so this command does not look for their files:

  $ cat > db/unmatched.json <<'JSON'
  > {
  >   "dialect": "mysql",
  >   "schema": ["tables/*.sql"],
  >   "migrations": {
  >     "lang": "ocaml",
  >     "schema": "schema.sql",
  >     "migrations": "migrations.sql",
  >     "module": "Migrations"
  >   }
  > }
  > JSON
  $ sqlgg migrations generate --config db/unmatched.json > unmatched.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ sqlgg -config db/unmatched.json
  db/unmatched.json: cannot read directory for schema "tables/*.sql": db/tables: No such file or directory
  [2]

Nor does it check the values of those keys. An error there is for code
generation to report:

  $ cat > db/bad_gen.json <<'JSON'
  > {
  >   "dialect": "mysql",
  >   "gen": "rust",
  >   "migrations": {
  >     "lang": "ocaml",
  >     "schema": "schema.sql",
  >     "migrations": "migrations.sql",
  >     "module": "Migrations"
  >   }
  > }
  > JSON
  $ sqlgg migrations generate --config db/bad_gen.json > bad_gen.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ sqlgg -config db/bad_gen.json
  db/bad_gen.json: expected "cxx" or "caml" or "caml_io" or "java" or "xml" or "json" or "csharp" or "sql" or "none" but got "rust"
  [2]
