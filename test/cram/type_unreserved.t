TYPE is an unreserved keyword: it must still work as a plain identifier in
every name position of the grammar (`ident`), in all dialects. Empty output
means everything parsed and type-checked.

Column named "type" through the whole DML lifecycle (MySQL, the common case in
the wild):

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE kw (type INT NOT NULL, val TEXT);
  > SELECT type, COUNT(*) FROM kw WHERE type > 0 GROUP BY type HAVING type < 10 ORDER BY type DESC LIMIT 1;
  > INSERT INTO kw (type, val) VALUES (1, 'a');
  > INSERT INTO kw SET type = 2;
  > INSERT INTO kw (type, val) VALUES (3, 'b') ON DUPLICATE KEY UPDATE type = VALUES(type);
  > UPDATE kw SET type = type + 1 WHERE type = 3;
  > DELETE FROM kw WHERE type = 4;
  > EOF

Table named "type", including qualified column access type.type:

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE type (id INT NOT NULL, type INT NOT NULL);
  > SELECT id FROM type;
  > SELECT type.id, type.type FROM type;
  > INSERT INTO type (id, type) VALUES (1, 2);
  > UPDATE type SET type = 2 WHERE type.id = 1;
  > DELETE FROM type WHERE id = 3;
  > DROP TABLE type;
  > EOF

Aliases and CTEs named "type":

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t1 (a INT NOT NULL);
  > SELECT a AS type FROM t1;
  > SELECT type.a FROM t1 AS type;
  > WITH type AS (SELECT a AS type FROM t1) SELECT type FROM type;
  > EOF

Keys, indexes and index names:

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE ix (type INT NOT NULL, val TEXT, PRIMARY KEY (type));
  > CREATE TABLE ix2 (type INT NOT NULL, UNIQUE (type));
  > CREATE INDEX type ON ix (type);
  > EOF

JOIN ... USING (type):

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE j1 (type INT NOT NULL, x INT NOT NULL);
  > CREATE TABLE j2 (type INT NOT NULL, y INT NOT NULL);
  > SELECT x, y FROM j1 JOIN j2 USING (type);
  > EOF

Every ALTER TABLE action that takes a name:

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE al (a INT NOT NULL);
  > ALTER TABLE al ADD COLUMN type INT;
  > ALTER TABLE al ADD INDEX idx_type (type);
  > ALTER TABLE al DROP INDEX idx_type;
  > ALTER TABLE al ADD INDEX type (type);
  > ALTER TABLE al DROP INDEX type;
  > ALTER TABLE al RENAME COLUMN type TO type2;
  > ALTER TABLE al RENAME COLUMN type2 TO type;
  > ALTER TABLE al CHANGE COLUMN type type BIGINT;
  > ALTER TABLE al MODIFY COLUMN type INT;
  > ALTER TABLE al ADD COLUMN b INT AFTER type;
  > ALTER TABLE al DROP COLUMN type;
  > CREATE TABLE type_tbl (a INT NOT NULL);
  > ALTER TABLE type_tbl RENAME TO type;
  > EOF

ALTER TABLE on a table named "type":

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE type (a INT NOT NULL);
  > ALTER TABLE type ADD COLUMN b INT;
  > DROP TABLE type;
  > EOF

Function parameter named "type":

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE FUNCTION f(type INT) RETURNS INT AS 'select 1';
  > EOF

Session variable assignment and function-call position (regression: `assign`
used a bare IDENT and rejected the TYPE token):

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > SET type = 1;
  > SELECT type(1);
  > EOF
  W: unknown function "type" of 1 arguments, treating as untyped

PostgreSQL: the hot zone where the TYPE keyword and the identifier meet.
A type named "type", a column named "type" of type "type", and
ALTER COLUMN type TYPE type in one place:

  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE type AS ENUM ('a', 'b');
  > CREATE TABLE pg (type type NOT NULL);
  > SELECT type FROM pg WHERE type = 'a';
  > ALTER TABLE pg ALTER COLUMN type TYPE type;
  > ALTER TABLE pg ALTER COLUMN type SET NOT NULL;
  > ALTER TABLE pg ALTER COLUMN type DROP NOT NULL;
  > ALTER TABLE pg ALTER COLUMN type SET DEFAULT 'a';
  > ALTER TABLE pg ALTER COLUMN type DROP DEFAULT;
  > DROP TYPE type;
  > EOF

SQLite (default keyword handling elsewhere must be untouched):

  $ sqlgg -gen none -dialect=sqlite - <<'EOF' 2>&1
  > CREATE TABLE kw (type INTEGER NOT NULL, val TEXT);
  > SELECT type FROM kw WHERE type = 1;
  > EOF

Code generation still works for a column named "type" (OCaml output must not
produce a bare `type` keyword clash):

  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE kw (type INT NOT NULL);
  > SELECT type FROM kw;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_kw db  =
      T.execute db ("CREATE TABLE kw (type INT NOT NULL)") T.no_params
  
    let select_1 db  callback =
      let invoke_callback stmt =
        callback
          ~type_:(T.get_column_Int stmt 0)
      in
      T.select db ("SELECT type FROM kw") T.no_params invoke_callback
  
    module Fold = struct
      let select_1 db  callback acc =
        let invoke_callback stmt =
          callback
            ~type_:(T.get_column_Int stmt 0)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT type FROM kw") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_1 db  callback =
        let invoke_callback stmt =
          callback
            ~type_:(T.get_column_Int stmt 0)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT type FROM kw") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)
