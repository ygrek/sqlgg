A hand-written manual migration can declare itself irreversible (no down) with
`-- [sqlgg] irreversible=true`. The generated revert is a real no-op: it runs no
SQL and returns zero affected rows, so the (apply, revert) shape stays uniform.

  $ cat > initial.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, legacy TEXT);
  > EOF

  $ cat > extends.sql <<'EOF'
  > -- [sqlgg] manual
  > -- [sqlgg] irreversible=true
  > -- [sqlgg] id=20251231
  > ALTER TABLE users DROP COLUMN legacy;
  > EOF

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > EOF

  $ : > migrations.sql
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name mig -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > output.ml
  appended 1 migration(s) to migrations.sql (id 20260101000000_alter_users_add_col_name)

The irreversible manual block (alter_users_0) gets a no-op revert; the generated
delta keeps a real one:

  $ cat output.ml
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_0 db  =
      T.execute db ("ALTER TABLE users DROP COLUMN legacy") T.no_params
  
    let revert_alter_users_0 db  =
      ignore db;
      IO.return { T.affected_rows = 0L; insert_id = None }
  
    let apply_20260101000000_alter_users_add_col_name db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `name` TEXT") T.no_params
  
    let revert_20260101000000_alter_users_add_col_name db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `name`") T.no_params
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
      ("20260101000000_alter_users_add_col_name", apply_20260101000000_alter_users_add_col_name, revert_20260101000000_alter_users_add_col_name);
    ]
  
  end (* module Mig *)

It type-checks against the traits and runs. Calling the no-op revert needs no
queued mock response - proof that it performs no database operation (a real
revert would raise on the empty mock queue):

  $ cp ../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ cat > run_mig.ml <<'EOF'
  > module I = Output.Mig (Print_impl)
  > let () =
  >   let r = I.revert_alter_users_0 () in
  >   Printf.printf "noop revert affected_rows=%Ld\n" r.Print_impl.affected_rows
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c run_mig.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -linkpkg -o run_mig.exe print_impl.ml output.ml run_mig.ml
  $ ./run_mig.exe
  noop revert affected_rows=0
