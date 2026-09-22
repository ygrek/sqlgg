CREATE EXTENSION, all optional clauses
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE EXTENSION pg_trgm;
  > CREATE EXTENSION IF NOT EXISTS pg_trgm;
  > CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;
  > CREATE EXTENSION pg_trgm SCHEMA public;
  > CREATE EXTENSION pg_trgm WITH SCHEMA public VERSION '1.6' CASCADE;
  > CREATE EXTENSION pg_trgm WITH VERSION unquoted_version;
  > EOF

DROP EXTENSION, all optional clauses
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > DROP EXTENSION pg_trgm;
  > DROP EXTENSION IF EXISTS pg_trgm;
  > DROP EXTENSION pg_trgm CASCADE;
  > DROP EXTENSION pg_trgm RESTRICT;
  > DROP EXTENSION IF EXISTS pg_trgm, btree_gin CASCADE;
  > EOF

sqlgg keeps no extension state, so an extension needs no prior CREATE to be
dropped, and may be created twice

  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > DROP EXTENSION pg_trgm;
  > CREATE EXTENSION pg_trgm;
  > CREATE EXTENSION pg_trgm;
  > EOF

Both are emitted as plain unprepared statements

  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;
  > DROP EXTENSION pg_trgm;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let statement_0 db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public") ~name:"statement_0" ~kind:Sqlgg_traits.Query.Other ())
  
    let statement_1 db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("DROP EXTENSION pg_trgm") ~name:"statement_1" ~kind:Sqlgg_traits.Query.Other ())
  
  end (* module Sqlgg *)

A following CTE statement is not mistaken for CREATE EXTENSION's WITH clause

  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (id INTEGER NOT NULL);
  > CREATE EXTENSION pg_trgm;
  > WITH c AS (SELECT id FROM t) SELECT id FROM c;
  > EOF

Extensions are PostgreSQL-only

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE EXTENSION pg_trgm;
  > EOF
  Feature Extension is not supported for dialect MySQL (supported by: PostgreSQL) at 
  Errors encountered, no code generated
  [1]

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > DROP EXTENSION pg_trgm;
  > EOF
  Feature Extension is not supported for dialect MySQL (supported by: PostgreSQL) at 
  Errors encountered, no code generated
  [1]

  $ sqlgg -gen none -dialect=mysql -no-check extension - <<'EOF' 2>&1
  > CREATE EXTENSION pg_trgm;
  > EOF
  Warning: Feature Extension is not supported for dialect MySQL, proceeding anyway at 

extension, schema and version stay unreserved: still usable as identifiers

  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE kw (extension TEXT, schema TEXT, version INTEGER NOT NULL);
  > SELECT extension, schema, version FROM kw WHERE schema = 'public';
  > CREATE EXTENSION schema;
  > EOF

They also stay usable in identifier positions that predate extension support

  $ sqlgg -gen none -dialect=postgresql -no-check=all - <<'EOF' 2>&1
  > CREATE TABLE kw_contexts (col TEXT COLLATE schema);
  > CREATE TABLE kw_charset (col TEXT CHARACTER SET extension);
  > CREATE FUNCTION extension(arg INTEGER) RETURNS INTEGER AS 'body' LANGUAGE version;
  > SELECT CONVERT('x' USING extension);
  > EOF
  Warning: Assuming custom collation implementation for PostgreSQL
