CREATE TYPE
  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > CREATE TABLE "shirt" ("id" INTEGER NOT NULL, "color" "color" NOT NULL);
  > SELECT "id" FROM "shirt" WHERE "color" = 'red';
  > DROP TYPE "color";
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_type_color db  =
      T.execute db ("CREATE TYPE \"color\" AS ENUM ('red', 'green', 'blue')") T.no_params
  
    let create_shirt db  =
      T.execute db ("CREATE TABLE \"shirt\" (\"id\" INTEGER NOT NULL, \"color\" \"color\" NOT NULL)") T.no_params
  
    let select_2 db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'red'") T.no_params invoke_callback
  
    let drop_type_color db  =
      T.execute db ("DROP TYPE \"color\"") T.no_params
  
    module Fold = struct
      let select_2 db  callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'red'") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_2 db  callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'red'") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Duplicate CREATE TYPE
  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > CREATE TYPE "color" AS ENUM ('black', 'white');
  > EOF
  Failed : CREATE TYPE "color" AS ENUM ('black', 'white')
  Fatal error: exception Failure("duplicate type declaration for \"color\"")
  [2]

DROP TYPE IF EXISTS is idempotent
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > DROP TYPE IF EXISTS "color";
  > DROP TYPE IF EXISTS "color";
  > EOF

Duplicate DROP TYPE
  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > DROP TYPE "color";
  > DROP TYPE "color";
  > EOF
  Failed : DROP TYPE "color"
  Fatal error: exception Failure("no such type \"color\"")
  [2]

CREATE DROP CREATE
  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > DROP TYPE "color";
  > CREATE TYPE "color" AS ENUM ('black', 'white');
  > CREATE TABLE "shirt" ("id" INTEGER NOT NULL, "color" "color" NOT NULL);
  > SELECT "id" FROM "shirt" WHERE "color" = 'black';
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_type_color db  =
      T.execute db ("CREATE TYPE \"color\" AS ENUM ('red', 'green', 'blue')") T.no_params
  
    let drop_type_color db  =
      T.execute db ("DROP TYPE \"color\"") T.no_params
  
    let create_type_color db  =
      T.execute db ("CREATE TYPE \"color\" AS ENUM ('black', 'white')") T.no_params
  
    let create_shirt db  =
      T.execute db ("CREATE TABLE \"shirt\" (\"id\" INTEGER NOT NULL, \"color\" \"color\" NOT NULL)") T.no_params
  
    let select_4 db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'black'") T.no_params invoke_callback
  
    module Fold = struct
      let select_4 db  callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'black'") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_4 db  callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT \"id\" FROM \"shirt\" WHERE \"color\" = 'black'") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)


CREATE TYPE misuse
  $ sqlgg -gen caml -no-header -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > CREATE TABLE "shirt" ("id" INTEGER NOT NULL, "color" "color" NOT NULL);
  > SELECT "id" FROM "shirt" WHERE "color" = 'black';
  > EOF
  Failed : SELECT "id" FROM "shirt" WHERE "color" = 'black'
  Fatal error: exception Failure("types Union (blue| green| red) and StringLiteral (black) for 'a do not match in 'a -> 'a -> Bool applied to (Union (blue| green| red), StringLiteral (black))")
  [2]

Only in PostgreSQL dialect
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TYPE "color" AS ENUM ('red', 'green', 'blue');
  > CREATE TABLE "shirt" ("id" INTEGER NOT NULL, "color" "color" NOT NULL);
  > EOF
  Feature UserDefinedType is not supported for dialect MySQL (supported by: PostgreSQL) at 
  Errors encountered, no code generated
  [1]

DROP TYPE is gated per-dialect too
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TYPE color AS ENUM ('red');
  > DROP TYPE color;
  > EOF
  Feature UserDefinedType is not supported for dialect MySQL (supported by: PostgreSQL) at 
  Feature UserDefinedType is not supported for dialect MySQL (supported by: PostgreSQL) at 
  Errors encountered, no code generated
  [1]

ALTER COLUMN ... TYPE is gated per-dialect (AlterColumn feature)
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (a INT NOT NULL);
  > ALTER TABLE t ALTER COLUMN a TYPE BIGINT;
  > EOF
  Feature AlterColumn is not supported for dialect MySQL (supported by: PostgreSQL) at TYPE BIGINT
  Errors encountered, no code generated
  [1]

TYPE is unreserved: still usable as a column name, even next to the TYPE keyword
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE kw (type INTEGER NOT NULL, val TEXT);
  > SELECT type FROM kw WHERE type = 1;
  > ALTER TABLE kw ALTER COLUMN type TYPE SMALLINT;
  > EOF

Unquoted CREATE TYPE / DROP TYPE
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TYPE color AS ENUM ('red', 'green', 'blue');
  > CREATE TABLE shirt (id INTEGER NOT NULL, c color NOT NULL);
  > SELECT id FROM shirt WHERE c = 'red';
  > DROP TYPE color;
  > EOF

Diff mode: the type registry is reset per schema replay, so the same
CREATE TYPE in both schemas must not clash as a duplicate
  $ cat > diff_initial.sql <<'EOF'
  > CREATE TYPE color AS ENUM ('red');
  > CREATE TABLE t (id INTEGER NOT NULL, c color NOT NULL);
  > EOF
  $ cat > diff_target.sql <<'EOF'
  > CREATE TYPE color AS ENUM ('red');
  > CREATE TABLE t (id INTEGER NOT NULL, c color NOT NULL, extra INTEGER);
  > EOF
  $ sqlgg -no-header -dialect postgresql -diff -now 20260101000000 -gen sql -base diff_initial.sql -target diff_target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_t_add_col_extra
  ALTER TABLE `t` ADD COLUMN `extra` INT;
  ALTER TABLE `t` DROP COLUMN `extra`;
