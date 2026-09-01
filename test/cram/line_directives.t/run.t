`-line-directives` makes the generated OCaml carry `# <line> "<file.sql>"`
directives, so that ocamlc and merlin report an error inside a generated query
function on the line of the query it came from, instead of on a line of the
machine-written .ml that nobody reads.

Start with the smallest possible file, and look at the whole thing.

  $ grep -n '' one.sql
  1:CREATE TABLE t (id INT NOT NULL);
  2:
  3:-- @erase
  4:DELETE FROM t WHERE id = @id;

Off by default, the output is exactly what it always was:

  $ sqlgg -no-header -gen caml one.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_t db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("CREATE TABLE t (id INT NOT NULL)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ())
  
    let erase db ~id =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("DELETE FROM t WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["t"]) ()) set_params
  
  end (* module Sqlgg *)

On, each generated line carries the line its query starts at -- 1 for the
CREATE, 4 for the DELETE, which is the statement itself and not the `-- @erase`
comment on line 3. Leaving a function, a second directive hands numbering back
to the generated file, so what follows is attributed to it and not to whatever
comes next in the .sql:

  $ sqlgg -no-header -gen caml -line-directives one.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "one.sql"
    let create_t db  =
  # 1 "one.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("CREATE TABLE t (id INT NOT NULL)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ())
  # 10 "one.sql.ml"
  
  # 4 "one.sql"
    let erase db ~id =
  # 4 "one.sql"
      let set_params stmt =
  # 4 "one.sql"
        let p = T.start_params stmt (1) in
  # 4 "one.sql"
        T.set_param_Int p id;
  # 4 "one.sql"
        T.finish_params p
  # 4 "one.sql"
      in
  # 4 "one.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("DELETE FROM t WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["t"]) ()) set_params
  # 26 "one.sql.ml"
  
  end (* module Sqlgg *)

No generator other than caml/caml_io emits anything, with or without the flag:

  $ for g in cxx java xml csharp; do
  >   sqlgg -no-header -line-directives -gen $g one.sql | grep -c '^# ' || true
  > done
  0
  0
  0
  0

Reading from stdin there is no file to point at, so nothing is emitted either:

  $ sqlgg -no-header -gen caml -line-directives - < one.sql | grep -c '^# ' || true
  0

`-line-directives-file` names the generated file explicitly, for build rules
whose target is not `<input>.ml`:

  $ sqlgg -no-header -gen caml -line-directives -line-directives-file sql_t.ml one.sql | grep '\.ml"'
  # 10 "sql_t.ml"
  # 26 "sql_t.ml"

Passing `-line-directives-file` without `-line-directives` is rejected:

  $ sqlgg -gen caml -line-directives-file sql_t.ml one.sql
  -line-directives-file requires -line-directives
  [1]

Several input files go into one generated module, so there is no single
`<input>.ml` to hand numbering back to and the generated file has to be named:

  $ sqlgg -gen caml -line-directives one.sql queries.sql
  -line-directives with several input files requires -line-directives-file
  [1]
  $ sqlgg -no-header -gen caml -line-directives -line-directives-file all.ml one.sql queries.sql | grep -o '"[^"]*\.ml"' | sort -u
  "all.ml"

Source paths are kept as passed to sqlgg, so that two inputs with the same
basename stay apart and a diagnostic can be resolved from the build workspace:

  $ mkdir -p dir-a dir-b
  $ echo 'CREATE TABLE first_table (id INT NOT NULL);' > dir-a/shared.sql
  $ echo 'CREATE TABLE second_table (id INT NOT NULL);' > dir-b/shared.sql
  $ sqlgg -no-header -gen caml -line-directives -line-directives-file build/all.ml dir-a/shared.sql dir-b/shared.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "dir-a/shared.sql"
    let create_first_table db  =
  # 1 "dir-a/shared.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"shared.sql" ~sql:("CREATE TABLE first_table (id INT NOT NULL)") ~name:"create_first_table" ~kind:Sqlgg_traits.Query.(Create "first_table") ())
  # 10 "build/all.ml"
  
  # 1 "dir-b/shared.sql"
    let create_second_table db  =
  # 1 "dir-b/shared.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"shared.sql" ~sql:("CREATE TABLE second_table (id INT NOT NULL)") ~name:"create_second_table" ~kind:Sqlgg_traits.Query.(Create "second_table") ())
  # 16 "build/all.ml"
  
  end (* module Sqlgg *)

The default name of the generated file keeps the directory of its input too:

  $ sqlgg -no-header -gen caml -line-directives dir-a/shared.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "dir-a/shared.sql"
    let create_first_table db  =
  # 1 "dir-a/shared.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"shared.sql" ~sql:("CREATE TABLE first_table (id INT NOT NULL)") ~name:"create_first_table" ~kind:Sqlgg_traits.Query.(Create "first_table") ())
  # 10 "dir-a/shared.sql.ml"
  
  end (* module Sqlgg *)

Dune's `copy_files#` starts the copy with a line directive of its own, naming
the file it was copied from. sqlgg follows it, so an error is reported against
the original and not against the copy in the build directory:

  $ cat > copied.sql <<'SQL'
  > # 1 "backend/shared/original.sql"
  > CREATE TABLE copied (
  >   -- [sqlgg] module=Fun
  >   id INT NOT NULL
  > );
  > -- @erase_copied
  > DELETE FROM copied WHERE id = @id;
  > SQL
  $ sqlgg -no-header -gen caml -line-directives copied.sql > copied.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c copied.ml
  File "backend/shared/original.sql", line 6, characters 27-40:
  Error: Unbound value Fun.set_param
  [2]

A directive holds until the next one. Only a `#` that starts a line is one,
anywhere else it is an ordinary comment:

  $ cat > spliced.sql <<'SQL'
  > # 1 "a.sql"
  > CREATE TABLE spliced (id INT NOT NULL); # 90 "not-a-directive.sql"
  > DELETE FROM spliced;
  > # 40 "b.sql"
  > DROP TABLE spliced;
  > SQL
  $ sqlgg -no-header -gen caml -line-directives spliced.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "a.sql"
    let create_spliced db  =
  # 1 "a.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"spliced.sql" ~sql:("CREATE TABLE spliced (id INT NOT NULL)") ~name:"create_spliced" ~kind:Sqlgg_traits.Query.(Create "spliced") ())
  # 10 "spliced.sql.ml"
  
  # 2 "a.sql"
    let delete_spliced_1 db  =
  # 2 "a.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"spliced.sql" ~sql:("DELETE FROM spliced") ~name:"delete_spliced_1" ~kind:Sqlgg_traits.Query.(Delete ["spliced"]) ())
  # 16 "spliced.sql.ml"
  
  # 40 "b.sql"
    let drop_spliced db  =
  # 40 "b.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"spliced.sql" ~sql:("DROP TABLE spliced") ~name:"drop_spliced" ~kind:Sqlgg_traits.Query.(Drop "spliced") ())
  # 22 "spliced.sql.ml"
  
  end (* module Sqlgg *)

Now a file with a multi-line query and several module kinds. Note that the SQL
stays one OCaml string literal with escaped newlines and no directive spliced
into it, and that `module Single = struct` / `end (* module Single *)` are not
preceded by one either:

  $ grep -n '' queries.sql
  1:CREATE TABLE person (
  2:  id INT NOT NULL,
  3:  name TEXT NOT NULL
  4:);
  5:
  6:-- @count_persons
  7:SELECT count(*) FROM person;
  8:
  9:-- @rename
  10:UPDATE person
  11:   SET name = @name
  12: WHERE id = @id;
  13:
  14:-- @erase
  15:DELETE FROM person WHERE id = @id;
  $ sqlgg -no-header -gen caml -line-directives queries.sql > out.ml
  $ cat out.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "queries.sql"
    let create_person db  =
  # 1 "queries.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("CREATE TABLE person (\n\
    id INT NOT NULL,\n\
    name TEXT NOT NULL\n\
  )") ~name:"create_person" ~kind:Sqlgg_traits.Query.(Create "person") ())
  # 13 "queries.sql.ml"
  
  # 7 "queries.sql"
    let count_persons db  =
  # 7 "queries.sql"
      let get_row stmt =
  # 7 "queries.sql"
        (T.get_column_Int stmt 0)
  # 7 "queries.sql"
      in
  # 7 "queries.sql"
      T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT count(*) FROM person") ~name:"count_persons" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params get_row
  # 25 "queries.sql.ml"
  
  # 10 "queries.sql"
    let rename db ~name ~id =
  # 10 "queries.sql"
      let set_params stmt =
  # 10 "queries.sql"
        let p = T.start_params stmt (2) in
  # 10 "queries.sql"
        T.set_param_Text p name;
  # 10 "queries.sql"
        T.set_param_Int p id;
  # 10 "queries.sql"
        T.finish_params p
  # 10 "queries.sql"
      in
  # 10 "queries.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("UPDATE person\n\
     SET name = @name\n\
   WHERE id = @id") ~name:"rename" ~kind:Sqlgg_traits.Query.(Update (Some "person")) ()) set_params
  # 45 "queries.sql.ml"
  
  # 15 "queries.sql"
    let erase db ~id =
  # 15 "queries.sql"
      let set_params stmt =
  # 15 "queries.sql"
        let p = T.start_params stmt (1) in
  # 15 "queries.sql"
        T.set_param_Int p id;
  # 15 "queries.sql"
        T.finish_params p
  # 15 "queries.sql"
      in
  # 15 "queries.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("DELETE FROM person WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["person"]) ()) set_params
  # 61 "queries.sql.ml"
  
    module Single = struct
  # 7 "queries.sql"
      let count_persons db  callback =
  # 7 "queries.sql"
        let invoke_callback stmt =
  # 7 "queries.sql"
          callback
  # 7 "queries.sql"
            ~r:(T.get_column_Int stmt 0)
  # 7 "queries.sql"
        in
  # 7 "queries.sql"
        T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT count(*) FROM person") ~name:"count_persons" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params invoke_callback
  # 76 "queries.sql.ml"
  
    end (* module Single *)
  end (* module Sqlgg *)
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c out.ml

Dynamic selects are generated through their own path -- a per-query column
module plus its `select` -- and get the same treatment:

  $ grep -n '' dyn.sql
  1:CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);
  2:
  3:-- [sqlgg] dynamic_select=true
  4:-- @pick
  5:SELECT id, name
  6:  FROM items
  7: WHERE id = @id;
  $ sqlgg -no-header -gen caml -line-directives dyn.sql > dyn.ml
  $ cat dyn.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Pick = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
  # 5 "dyn.sql"
        let id : _ t =
  # 5 "dyn.sql"
          {
  # 5 "dyn.sql"
            set = (fun _p -> ());
  # 5 "dyn.sql"
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
  # 5 "dyn.sql"
            column = ("id");
  # 5 "dyn.sql"
            count = 0;
  # 5 "dyn.sql"
            deps = [];
  # 5 "dyn.sql"
          }
  # 5 "dyn.sql"
        let name : _ t =
  # 5 "dyn.sql"
          {
  # 5 "dyn.sql"
            set = (fun _p -> ());
  # 5 "dyn.sql"
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
  # 5 "dyn.sql"
            column = ("name");
  # 5 "dyn.sql"
            count = 0;
  # 5 "dyn.sql"
            deps = [];
  # 5 "dyn.sql"
          }
  # 41 "dyn.sql.ml"
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
      end
  
  # 5 "dyn.sql"
      let select db (col : _ t) ~id =
  # 5 "dyn.sql"
        let set_params stmt =
  # 5 "dyn.sql"
          let p = T.start_params stmt (1 + col.count) in
  # 5 "dyn.sql"
          col.set p;
  # 5 "dyn.sql"
          T.set_param_Int p id;
  # 5 "dyn.sql"
          T.finish_params p
  # 5 "dyn.sql"
        in
  # 5 "dyn.sql"
        T.select_one_maybe db
  # 5 "dyn.sql"
        (Sqlgg_traits.Query.make ~filename:"dyn.sql" ~sql:("SELECT " ^ col.column ^ "\n\
    FROM items\n\
   WHERE id = @id") ~name:"pick" ~kind:Sqlgg_traits.Query.(Select Zero_one) ())
  # 5 "dyn.sql"
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  # 71 "dyn.sql.ml"
  
    end
  
  
  # 1 "dyn.sql"
    let create_items db  =
  # 1 "dyn.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"dyn.sql" ~sql:("CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL)") ~name:"create_items" ~kind:Sqlgg_traits.Query.(Create "items") ())
  # 80 "dyn.sql.ml"
  
    module Single = struct
    end (* module Single *)
  end (* module Sqlgg *)
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c dyn.ml

Multi-row dynamic selects pass each argument to `callback` on its own line, and
every one of those lines is pinned to the query line as well:

  $ cat > dyn_many.sql <<'SQL'
  > CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);
  > 
  > -- [sqlgg] dynamic_select=true
  > -- @list_items
  > SELECT id, name
  >   FROM items;
  > SQL
  $ sqlgg -no-header -gen caml -line-directives dyn_many.sql > dyn_many.ml
  $ grep -B 1 '__sqlgg_r_col' dyn_many.ml
  # 5 "dyn_many.sql"
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
  # 5 "dyn_many.sql"
            __sqlgg_r_col)
  --
  # 5 "dyn_many.sql"
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
  # 5 "dyn_many.sql"
            __sqlgg_r_col !r_acc)))
  --
  # 5 "dyn_many.sql"
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c dyn_many.ml

The mapping has to hold for the whole function body, which is much longer than
the query it came from. Here `big` starts on line 9 and `after` already on 15,
while the body of `big` runs for a good deal more than six lines. Column `bad`
carries `module=Fun`, which has no `set_param`, and is bound by the last
parameter, so the broken line sits at the very end of that body:

  $ grep -n '' long.sql
  1:CREATE TABLE wide (
  2:  c1 INT NOT NULL, c2 INT NOT NULL, c3 INT NOT NULL, c4 INT NOT NULL,
  3:  c5 INT NOT NULL, c6 INT NOT NULL, c7 INT NOT NULL, c8 INT NOT NULL,
  4:  -- [sqlgg] module=Fun
  5:  bad INT NOT NULL
  6:);
  7:
  8:-- @big
  9:UPDATE wide
  10:   SET c1 = @p1, c2 = @p2, c3 = @p3, c4 = @p4,
  11:       c5 = @p5, c6 = @p6, c7 = @p7, c8 = @p8
  12: WHERE bad = @bad;
  13:
  14:-- @after
  15:DELETE FROM wide;
  $ sqlgg -no-header -gen caml -line-directives long.sql > long.ml
  $ cat long.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "long.sql"
    let create_wide db  =
  # 1 "long.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("CREATE TABLE wide (\n\
    c1 INT NOT NULL, c2 INT NOT NULL, c3 INT NOT NULL, c4 INT NOT NULL,\n\
    c5 INT NOT NULL, c6 INT NOT NULL, c7 INT NOT NULL, c8 INT NOT NULL,\n\
      bad INT NOT NULL\n\
  )") ~name:"create_wide" ~kind:Sqlgg_traits.Query.(Create "wide") ())
  # 14 "long.sql.ml"
  
  # 9 "long.sql"
    let big db ~p1 ~p2 ~p3 ~p4 ~p5 ~p6 ~p7 ~p8 ~bad =
  # 9 "long.sql"
      let set_params stmt =
  # 9 "long.sql"
        let p = T.start_params stmt (9) in
  # 9 "long.sql"
        T.set_param_Int p p1;
  # 9 "long.sql"
        T.set_param_Int p p2;
  # 9 "long.sql"
        T.set_param_Int p p3;
  # 9 "long.sql"
        T.set_param_Int p p4;
  # 9 "long.sql"
        T.set_param_Int p p5;
  # 9 "long.sql"
        T.set_param_Int p p6;
  # 9 "long.sql"
        T.set_param_Int p p7;
  # 9 "long.sql"
        T.set_param_Int p p8;
  # 9 "long.sql"
        T.set_param_int64 p (Fun.set_param bad);
  # 9 "long.sql"
        T.finish_params p
  # 9 "long.sql"
      in
  # 9 "long.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("UPDATE wide\n\
     SET c1 = @p1, c2 = @p2, c3 = @p3, c4 = @p4,\n\
         c5 = @p5, c6 = @p6, c7 = @p7, c8 = @p8\n\
   WHERE bad = @bad") ~name:"big" ~kind:Sqlgg_traits.Query.(Update (Some "wide")) ()) set_params
  # 49 "long.sql.ml"
  
  # 15 "long.sql"
    let after db  =
  # 15 "long.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("DELETE FROM wide") ~name:"after" ~kind:Sqlgg_traits.Query.(Delete ["wide"]) ())
  # 55 "long.sql.ml"
  
  end (* module Sqlgg *)

A single directive per function would have numbered the broken line past the end
of long.sql; with a shorter body it would have landed on `after`. Re-pinning
every line keeps it on 9:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c long.ml
  File "long.sql", line 9, characters 27-40:
  Error: Unbound value Fun.set_param
  [2]

Without the flag the same error is reported against the generated file, as before:

  $ sqlgg -no-header -gen caml long.sql > long_plain.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c long_plain.ml
  File "long_plain.ml", line 23, characters 27-40:
  23 |       T.set_param_int64 p (Fun.set_param bad);
                                  ^^^^^^^^^^^^^
  Error: Unbound value Fun.set_param
  [2]

Migrations: `apply_*` and `revert_*` come from two separate SQL blocks and are
mapped onto their own, here lines 3 and 5 of extends.sql:

  $ grep -n '' extends.sql
  1:-- [sqlgg] manual
  2:-- [sqlgg] id=20260609000000
  3:ALTER TABLE users
  4:  RENAME COLUMN email TO email_address;
  5:ALTER TABLE users
  6:  RENAME COLUMN email_address TO email;
`-migrate` reads several SQL files, so it needs `-line-directives-file` too:

  $ sqlgg -dialect mysql -migrate -gen caml -line-directives -initial initial.sql -target target.sql
  -line-directives with -migrate requires -line-directives-file
  [1]
  $ mkdir -p migration-source
  $ cp extends.sql migration-source/extends.sql
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations \
  >   -line-directives -line-directives-file migrations.ml \
  >   -initial initial.sql -extends migration-source/extends.sql -target target.sql
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
  # 3 "migration-source/extends.sql"
    let apply_alter_users_0 db  =
  # 3 "migration-source/extends.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
    RENAME COLUMN email TO email_address") ~name:"apply_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
  # 11 "migrations.ml"
  
  # 5 "migration-source/extends.sql"
    let revert_alter_users_0 db  =
  # 5 "migration-source/extends.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
    RENAME COLUMN email_address TO email") ~name:"revert_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
  # 18 "migrations.ml"
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
    ]
  
  end (* module Migrations *)
  nothing new to migrate; regenerated code from 1 recorded migration(s)

A `down` given inline as a property has no SQL block of its own, so `revert_*`
is mapped onto the line of the `up` it belongs to:

  $ cat > inline_down.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=20260609000000
  > -- [sqlgg] down="ALTER TABLE users RENAME COLUMN email_address TO email"
  > ALTER TABLE users
  >   RENAME COLUMN email TO email_address;
  > SQL
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations \
  >   -line-directives -line-directives-file migrations.ml \
  >   -initial initial.sql -extends inline_down.sql -target target.sql 2> /dev/null \
  >   | grep -B 1 -E 'let (apply|revert)_'
  # 4 "inline_down.sql"
    let apply_alter_users_0 db  =
  --
  # 4 "inline_down.sql"
    let revert_alter_users_0 db  =

`sqlgg migrations generate --line-directives` attributes each generated or
manual step to its own SQL file and uses the `--output` path when handing
line numbering back to the generated module, so it cannot write to stdout:

  $ sqlgg migrations generate --dialect mysql --lang ocaml \
  >   --migrations migrations.sql --schema target.sql --module Migrations --line-directives
  sqlgg migrations: --line-directives requires --output
  [2]

  $ cat > target_more.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY,
  >   email_address VARCHAR(255) NOT NULL,
  >   bio TEXT NULL
  > );
  > SQL
  $ mkdir -p sub
  $ sqlgg migrations generate --dialect mysql --lang ocaml \
  >   --initial initial.sql --manual extends.sql --migrations migrations.sql \
  >   --schema target_more.sql --module Migrations --now 20260701000000 \
  >   --output sub/mig.ml --line-directives
  appended 1 migration(s) to migrations.sql (id 20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk)
  $ grep -n '' migrations.sql
  1:-- [sqlgg] generated
  2:-- [sqlgg] id=20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk
  3:ALTER TABLE `users` ADD COLUMN `bio` TEXT NULL, CHANGE COLUMN `email_address` `email_address` VARCHAR(255) NOT NULL, ADD PRIMARY KEY (`id`);
  4:ALTER TABLE `users` DROP COLUMN `bio`, CHANGE COLUMN `email_address` `email_address` TEXT NOT NULL, DROP PRIMARY KEY;
  $ nl -ba sub/mig.ml
       1	(* DO NOT EDIT MANUALLY *)
       2	(*  *)
       3	(* generated by sqlgg *)
       4	
       5	module Migrations (T : Sqlgg_traits.M_io) = struct
       6	
       7	
       8	# 3 "extends.sql"
       9	  let apply_alter_users_0 db  =
      10	# 3 "extends.sql"
      11	    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
      12	  RENAME COLUMN email TO email_address") ~name:"apply_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
      13	# 14 "sub/mig.ml"
      14	
      15	# 5 "extends.sql"
      16	  let revert_alter_users_0 db  =
      17	# 5 "extends.sql"
      18	    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
      19	  RENAME COLUMN email_address TO email") ~name:"revert_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
      20	# 21 "sub/mig.ml"
      21	
      22	# 3 "migrations.sql"
      23	  let apply_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk db  =
      24	# 3 "migrations.sql"
      25	    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` ADD COLUMN `bio` TEXT NULL, CHANGE COLUMN `email_address` `email_address` VARCHAR(255) NOT NULL, ADD PRIMARY KEY (`id`)") ~name:"apply_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk" ~kind:Sqlgg_traits.Query.Other ())
      26	# 27 "sub/mig.ml"
      27	
      28	# 4 "migrations.sql"
      29	  let revert_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk db  =
      30	# 4 "migrations.sql"
      31	    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP COLUMN `bio`, CHANGE COLUMN `email_address` `email_address` TEXT NOT NULL, DROP PRIMARY KEY") ~name:"revert_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk" ~kind:Sqlgg_traits.Query.Other ())
      32	# 33 "sub/mig.ml"
      33	
      34	  let baseline = None
      35	
      36	  let migration_plan :
      37	      ([ `WR ] T.connection -> T.execute_response T.IO.future)
      38	      Sqlgg_migrations.Migration.t
      39	      list =
      40	    [
      41	      Sqlgg_migrations.Migration.reversible
      42	        (Sqlgg_migrations.Migration_id.parse_generated ~id:"20260609000000")
      43	        ~apply:apply_alter_users_0
      44	        ~revert:revert_alter_users_0;
      45	      Sqlgg_migrations.Migration.reversible
      46	        (Sqlgg_migrations.Migration_id.parse_generated ~id:"20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk")
      47	        ~apply:apply_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk
      48	        ~revert:revert_20260701000000_alter_users_add_col_bio_change_col_email_address_add_pk;
      49	    ]
      50	
      51	end (* module Migrations *)
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.migrations -I sub -c sub/mig.ml

A migration log can be read from its source path and written to a separate
build target. Both migration CLIs keep the source untouched and attribute the
generated migration to the source path:

  $ sqlgg -dialect mysql -migrate -gen caml -migrations-output orphan.sql \
  >   -initial initial.sql -target initial.sql
  -migrations-output requires -migrations-file
  [1]

  $ mkdir legacy-output
  $ sqlgg -no-header -dialect mysql -migrate -now 20260702000000 -gen caml_io -name Migrations \
  >   -line-directives -line-directives-file legacy-output/migrations.ml.check \
  >   -initial initial.sql -migrations-file legacy-output/migrations.sql \
  >   -migrations-output legacy-output/migrations.sql.check -target target.sql \
  >   > legacy-output/migrations.ml.check 2> /dev/null
  $ test ! -e legacy-output/migrations.sql && echo source-untouched
  source-untouched
  $ cat legacy-output/migrations.sql.check
  -- [sqlgg] generated
  -- [sqlgg] id=20260702000000_alter_users_drop_col_email_add_col_email_address
  ALTER TABLE `users` DROP COLUMN `email`, ADD COLUMN `email_address` TEXT NOT NULL;
  ALTER TABLE `users` DROP COLUMN `email_address`, ADD COLUMN `email` TEXT NOT NULL;
  $ cat legacy-output/migrations.ml.check
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
  # 3 "legacy-output/migrations.sql"
    let apply_20260702000000_alter_users_drop_col_email_add_col_email_address db  =
  # 3 "legacy-output/migrations.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP COLUMN `email`, ADD COLUMN `email_address` TEXT NOT NULL") ~name:"apply_20260702000000_alter_users_drop_col_email_add_col_email_address" ~kind:Sqlgg_traits.Query.Other ())
  # 10 "legacy-output/migrations.ml.check"
  
  # 4 "legacy-output/migrations.sql"
    let revert_20260702000000_alter_users_drop_col_email_add_col_email_address db  =
  # 4 "legacy-output/migrations.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP COLUMN `email_address`, ADD COLUMN `email` TEXT NOT NULL") ~name:"revert_20260702000000_alter_users_drop_col_email_add_col_email_address" ~kind:Sqlgg_traits.Query.Other ())
  # 16 "legacy-output/migrations.ml.check"
  
    let migrations = [
      ("20260702000000_alter_users_drop_col_email_add_col_email_address", apply_20260702000000_alter_users_drop_col_email_add_col_email_address, revert_20260702000000_alter_users_drop_col_email_add_col_email_address);
    ]
  
  end (* module Migrations *)
  $ cp legacy-output/migrations.sql.check legacy-output/migrations.sql
  $ sqlgg -no-header -dialect mysql -migrate -gen caml_io -name Migrations \
  >   -initial initial.sql -migrations-file legacy-output/migrations.sql \
  >   -migrations-output legacy-output/migrations-again.sql.check -target target.sql \
  >   > /dev/null 2> /dev/null
  $ cmp legacy-output/migrations.sql legacy-output/migrations-again.sql.check \
  >   && echo unchanged-log-copied
  unchanged-log-copied

  $ mkdir command-output
  $ sqlgg migrations generate --dialect mysql --lang ocaml --module Migrations \
  >   --line-directives --output command-output/migrations.ml.check \
  >   --initial initial.sql --migrations command-output/migrations.sql \
  >   --migrations-output command-output/migrations.sql.check --schema target.sql \
  >   --now 20260702000000 --ddl-as-migration 2> /dev/null
  $ test ! -e command-output/migrations.sql && echo source-untouched
  source-untouched
  $ cat command-output/migrations.sql.check
  -- [sqlgg] generated
  -- [sqlgg] id=20260702000000_alter_users_drop_col_email_add_col_email_address
  ALTER TABLE `users` DROP COLUMN `email`, ADD COLUMN `email_address` TEXT NOT NULL;
  ALTER TABLE `users` DROP COLUMN `email_address`, ADD COLUMN `email` TEXT NOT NULL;
  $ cat command-output/migrations.ml.check
  (* DO NOT EDIT MANUALLY *)
  (*  *)
  (* generated by sqlgg *)
  
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
  
  # 3 "command-output/migrations.sql"
    let apply_20260702000000_alter_users_drop_col_email_add_col_email_address db  =
  # 3 "command-output/migrations.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP COLUMN `email`, ADD COLUMN `email_address` TEXT NOT NULL") ~name:"apply_20260702000000_alter_users_drop_col_email_add_col_email_address" ~kind:Sqlgg_traits.Query.Other ())
  # 13 "command-output/migrations.ml.check"
  
  # 4 "command-output/migrations.sql"
    let revert_20260702000000_alter_users_drop_col_email_add_col_email_address db  =
  # 4 "command-output/migrations.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP COLUMN `email_address`, ADD COLUMN `email` TEXT NOT NULL") ~name:"revert_20260702000000_alter_users_drop_col_email_add_col_email_address" ~kind:Sqlgg_traits.Query.Other ())
  # 19 "command-output/migrations.ml.check"
  
    let baseline = None
  
    let migration_plan :
        ([ `WR ] T.connection -> T.execute_response T.IO.future)
        Sqlgg_migrations.Migration.t
        list =
      [
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20260702000000_alter_users_drop_col_email_add_col_email_address")
          ~apply:apply_20260702000000_alter_users_drop_col_email_add_col_email_address
          ~revert:revert_20260702000000_alter_users_drop_col_email_add_col_email_address;
      ]
  
  end (* module Migrations *)
