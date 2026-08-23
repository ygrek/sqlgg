sqlgg as a dune dialect: every .sql file in the project becomes an OCaml
module, preprocessed by `sqlgg -open ddl.sql -open shared.sql <file>`.

The -open'ed files provide the schema and reusable queries to every module
without their code being duplicated into each of them (because of -open).

The project is committed as a fixture next to this file: ddl.sql (schema only),
shared.sql (a reusable query only), a.sql and b.sql (plain queries), wired up
by the dialect stanza. %{dep:...} is required for the -open'ed files -- dialect
preprocess actions are sandboxed with only %{input-file} present, so plain
filenames would not resolve:

  $ cat dune-project
  (lang dune 3.9)
  (dialect
   (name sqlgg)
   (implementation
    (extension sql)
    (preprocess
     (run sqlgg -no-header -open %{dep:ddl.sql} -open %{dep:shared.sql} -gen caml %{input-file}))))

  $ cat dune
  (library
   (name queries)
   (libraries sqlgg.traits))

  $ dune build --root .

Each module contains exactly the functions of its own file, Shared is empty:

  $ for f in ddl shared a b; do echo "$f:"; grep -coE '^  let [a-z_]+ db' _build/default/$f.sql.ml; grep -oE '^  let [a-z_]+ db' _build/default/$f.sql.ml | sed 's/^ *//'; done
  ddl:
  1
  let create_person db
  shared:
  0
  a:
  1
  let count_persons db
  b:
  1
  let list_adults db
