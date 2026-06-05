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
